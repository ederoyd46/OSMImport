{-# LANGUAGE OverloadedStrings #-}

module Importer where

import OSMFormat
import Common
import qualified Database as MDB
import qualified Redis as R
import qualified Data.Serialize as S
import Data.ProtocolBuffers
import Control.Concurrent (forkIO, MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad(when, forever)
import GHC.Generics (Generic)
import Data.Maybe(fromJust, isJust, isNothing)
import Data.Binary.Get
import Codec.Compression.Zlib
import System.Exit
import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BCE
import Data.List.Split

showUsage = do
      hPutStrLn stderr "usage: dbconnection dbname filename"
      hPutStrLn stderr "example: OSMImport mongo '127.0.0.1:7720' 'geo_data' './download/england-latest.osm.pbf'"
      hPutStrLn stderr "example: OSMImport redis '127.0.0.1:7721' '2' './download/england-latest.osm.pbf'"
      exitFailure

data Chunk = Chunk {
    blob_header :: BlobHeader,
    blob :: Blob
} deriving (Show)

getChunks :: Integral a => a -> a -> [Chunk] -> Get [Chunk]
getChunks limit location chunks
  | limit > location = do
    len <- getWord32be
    headerBytes <- getByteString (fromIntegral len)
    let Right blobHeader = S.runGet decodeMessage =<< Right headerBytes :: Either String BlobHeader
    blobData <- getByteString (fromIntegral $ (getField $ bh_datasize blobHeader) :: Int)
    let Right blob = S.runGet decodeMessage =<< Right blobData :: Either String Blob
    bytesRead <- bytesRead
    let location = fromIntegral bytesRead
    getChunks limit location ((Chunk blobHeader blob) : chunks)
  | otherwise = return $ reverse chunks

startImport :: String -> String -> String -> String -> IO ()
startImport dbtype dbconnection dbname filename = do
  let host = (splitOn ":" dbconnection) !! 0
  let port = read ((splitOn ":" dbconnection) !! 1) :: Int
  
  let dbcommand recs = case dbtype of
                          "mongo" -> MDB.saveNodes dbconnection dbname recs
                          "redis" -> R.saveNodes host port (read dbname :: Int) recs
                          _ -> showUsage
                          
  performImport filename dbcommand
  return ()

performImport :: FilePath -> ([ImportNode] -> IO ()) -> IO ()
performImport fileName dbcommand = do
  dbMVar <- newEmptyMVar
  forkIO $ forkedDBCommand dbMVar
  handle <- BS.readFile fileName
  let fileLength = fromIntegral $ BS.length handle
  let chunks = runGet (getChunks fileLength (0 :: Integer) []) handle
  putStrLn $ "File Length : [" ++ (show fileLength) ++ "] Contains: [" ++ (show (length chunks)) ++ "] chunks"
  processData chunks [] 1 dbMVar
    where
      forkedDBCommand dbMVar = do
        forever $ do
          recs <- takeMVar dbMVar
          putStrLn "Running command in seperate thread"
          dbcommand recs

      processData [] [] _ _ = return ()
      processData [] y _ dbMVar = do 
        putStrLn $ "Final Database Checkpoint"
        putMVar dbMVar y
      processData x y z dbMVar 
         | length y > 50000 = do 
             putStrLn $ "Database Checkpoint"
             putMVar dbMVar y
             processData x [] z dbMVar
      processData (x:xs) y count dbMVar = do
        let blobCompressed = fromJust $ getField $ b_zlib_data (blob x)
        let blobUncompressed = decompress . BS.fromChunks $ [blobCompressed]
        let btype = getField $ bh_type (blob_header x)
        case btype of
          "OSMHeader" -> do 
            let Right headerBlock = S.runGetLazy decodeMessage =<< Right blobUncompressed :: Either String HeaderBlock
            -- let features = getField $ hb_required_features headerBlock
            let bbox = fromJust $ getField $ hb_bbox headerBlock
            let minlat = (fromIntegral $ getField $ hbbox_bottom bbox) / nano
            let minlon = (fromIntegral $ getField $ hbbox_left bbox) / nano
            let maxlat = (fromIntegral $ getField $ hbbox_top bbox) / nano
            let maxlon = (fromIntegral $ getField $ hbbox_right bbox) / nano
            putStrLn $ "Bounding Box (lat, lon): (" ++ (show minlat) ++ "," ++ (show minlon) ++ ") (" ++ (show maxlat) ++ "," ++ (show maxlon) ++ ")"
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Header data"
            processData xs y (count + 1) dbMVar
          "OSMData" -> do
            let eitherDataBlock = S.runGetLazy decodeMessage =<< Right blobUncompressed :: Either String PrimitiveBlock
            case eitherDataBlock of
              Left msg -> do
                putStrLn $ "Chunk : [" ++ (show count) ++ "] Not a parsable node: " ++ msg
                processData xs y (count + 1) dbMVar
              Right dataBlock -> do
                let stringTable = map BCE.unpack (getField $ st_bytes (getField $ pb_stringtable dataBlock))
                let granularity = fromIntegral $ fromJust . getField $ pb_date_granularity dataBlock
                let pg = getField $ pb_primitivegroup dataBlock
                nodes <- primitiveGroups pg [] stringTable granularity
                let nodeCount = length nodes
                putStrLn $ "Chunk : [" ++ (show count) ++ "] Nodes parsed = [" ++ (show nodeCount) ++ "]"
                processData xs (y ++ nodes) (count + 1) dbMVar

      -- Primitive Groups
      primitiveGroups :: [PrimitiveGroup] -> [ImportNode] -> [String] -> Integer -> IO [ImportNode]
      primitiveGroups [] [] _ _ = return []
      primitiveGroups [] y _ _ = return y
      primitiveGroups (x:xs) y stringTable granularity = do 
        let s = getField $ pg_dense x
        case s of
          Just nodes -> do
            let impNodes = denseNodes (fromJust s)
            primitiveGroups xs (y ++ impNodes) stringTable granularity
          Nothing -> primitiveGroups xs y stringTable granularity
        where
          denseNodes :: DenseNodes -> [ImportNode]
          denseNodes d = do 
            let ids = map fromIntegral (getField $ dense_nodes_id d)
            let latitudes = map fromIntegral (getField $ dense_nodes_lat d)
            let longitudes = map fromIntegral (getField $ dense_nodes_lon d)
            let keyvals = map fromIntegral (getField $ dense_nodes_keys_vals d)
            
            let maybeInfo = getField $ dense_nodes_info d
            case maybeInfo of
              Just info -> do 
                let versions = map fromIntegral (getField $ dense_info_version info) 
                let timestamps = map fromIntegral (getField $ dense_info_timestamp info) 
                let changesets = map fromIntegral (getField $ dense_info_changeset info) 
                let uids = map fromIntegral (getField $ dense_info_uid info) 
                let sids = map fromIntegral (getField $ dense_info_user_sid info) 
                buildNodeData ids latitudes longitudes keyvals versions timestamps changesets uids sids
              Nothing -> buildNodeData ids latitudes longitudes keyvals [] [] [] [] []

          buildNodeData :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [ImportNode]
          buildNodeData ids lat lon keyvals versions timestamps changesets uids sids = do
            let identifiers = deltaDecode ids 0 []
            let latitudes = calculateDegrees (deltaDecode lat 0 []) [] granularity 
            let longitudes = calculateDegrees (deltaDecode lon 0 []) [] granularity 
            --Need to rename all these
            let decodedTimestamps = deltaDecode timestamps 0 []
            let decodedChangesets = deltaDecode changesets 0 []
            let decodedUIDs = deltaDecode uids 0 []
            let decodedUsers = deltaDecode sids 0 []
            
            buildNodes identifiers latitudes longitudes keyvals versions decodedTimestamps decodedChangesets decodedUIDs decodedUsers []

          buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [ImportNode] -> [ImportNode]
          buildNodes [] [] [] [] [] [] [] [] [] [] = []
          buildNodes [] [] [] [] [] [] [] [] [] nodes = nodes
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals [] [] [] [] [] nodes = do
            let buildNode = ImportNode {_id=id, latitude=lat, longitude=long, tags=(fst nextKeyVals), version=Nothing, timestamp=Nothing, changeset=Nothing, uid=Nothing, sid=Nothing}
            buildNodes ids lats longs (snd nextKeyVals) [] [] [] [] [] (buildNode : nodes)
            where 
              nextKeyVals = splitKeyVal keyvals []

          buildNodes (id:ids) (lat:lats) (long:longs) keyvals (ver:versions) (ts:timestamps) (cs:changesets) (uid:uids) (sid:sids) nodes = do
            let buildNode = ImportNode {_id=id, latitude=lat, longitude=long, tags=(fst nextKeyVals), version=(Just ver), timestamp=(Just ts), changeset=(Just cs), uid=(Just uid), sid=(Just (stringTable !! (fromIntegral sid :: Int)))}
            buildNodes ids lats longs (snd nextKeyVals) versions timestamps changesets uids sids (buildNode : nodes)
            where 
              nextKeyVals = splitKeyVal keyvals []

          calculateDegrees :: [Integer] -> [Float] -> Integer -> [Float]
          calculateDegrees [] [] gran = []
          calculateDegrees [] y gran = reverse y
          calculateDegrees (x:xs) y gran = do
            let newcoordinate = fromIntegral (x * 100) / nano
            calculateDegrees xs (newcoordinate : y) gran

          splitKeyVal :: [Integer] -> [ImportTag] -> ([ImportTag], [Integer])
          splitKeyVal [] [] = ([], [])
          splitKeyVal [] y = (y, [])
          splitKeyVal (x:xx:xs) y 
            | x == 0 = (y, (xx : xs))
            | otherwise = splitKeyVal xs (ImportTag (stringTable !! (fromIntegral x :: Int)) (stringTable !! (fromIntegral xx :: Int)) : y)
          splitKeyVal (x:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0
          
          
