{-# LANGUAGE OverloadedStrings #-}

module Importer where

import Common
import Data.Node
import Data.Tag
import qualified Database as MDB
import qualified Redis as R
import qualified Data.Serialize as S
import Control.Concurrent (forkIO, MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad(when, forever)
import Data.Maybe(fromJust, isJust, isNothing)
import Data.Binary.Get(Get, getWord32be, getLazyByteString, runGet, bytesRead)
import Codec.Compression.Zlib
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy as BLAZY
import Data.List.Split

import Text.ProtocolBuffers.Basic
import qualified Data.Foldable as F(forM_,toList)
import qualified Data.ByteString.Lazy.UTF8 as U(toString)

import Text.ProtocolBuffers(messageGet,utf8,isSet,getVal)
import Text.DescriptorProtos.FileDescriptorProto

import OSM.FileFormat.Blob
import OSM.FileFormat.BlockHeader
import OSM.OSMFormat.HeaderBlock 
import OSM.OSMFormat.PrimitiveBlock
import OSM.OSMFormat.HeaderBBox
import OSM.OSMFormat.StringTable
import OSM.OSMFormat.DenseInfo
import OSM.OSMFormat.DenseNodes
import OSM.OSMFormat.PrimitiveGroup

showUsage = do
      hPutStrLn stderr "usage: dbconnection dbname filename"
      hPutStrLn stderr "example: OSMImport mongo '127.0.0.1:7720' 'geo_data' './download/england-latest.osm.pbf'"
      hPutStrLn stderr "example: OSMImport redis '127.0.0.1:7721' '2' './download/england-latest.osm.pbf'"
      exitFailure

data Chunk = Chunk {
    blob_header :: BlockHeader
  , blob :: Blob
} deriving (Show)

getChunks :: Integral a => a -> a -> [Chunk] -> Get [Chunk]
getChunks limit location chunks
  | limit > location = do
    len <- getWord32be
    headerBytes <- getLazyByteString (fromIntegral len)
    let Right (blobHeader,_) = messageGet headerBytes ::  Either String (BlockHeader, ByteString)
    blobData <- getLazyByteString $ fromIntegral $ getVal blobHeader datasize
    let Right (blob,_) = messageGet blobData :: Either String (Blob, ByteString)
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
  handle <- BLAZY.readFile fileName
  let fileLength = fromIntegral $ BLAZY.length handle
  let chunks = runGet (getChunks fileLength (0 :: Integer) []) handle
  putStrLn $ "File Length : [" ++ (show fileLength) ++ "] Contains: [" ++ (show (length chunks)) ++ "] chunks"
  processData chunks 1 dbMVar
    where
      forkedDBCommand dbMVar = do
        forever $ do
          recs <- takeMVar dbMVar
          dbcommand recs
 
      processData [] _ _ = return ()
      processData (x:xs) count dbMVar = do
        let blobUncompressed = decompress $ getVal (blob x) zlib_data
        let btype = getVal (blob_header x) type'
        case uToString btype of
          "OSMHeader" -> do
            let Right (headerBlock, _) = messageGet blobUncompressed :: Either String (HeaderBlock, BLAZY.ByteString)
            let b = getVal headerBlock bbox
            let minlat = (fromIntegral $ getVal b bottom) / nano
            let minlon = (fromIntegral $ getVal b left) / nano
            let maxlat = (fromIntegral $ getVal b top) / nano
            let maxlon = (fromIntegral $ getVal b right) / nano
            putStrLn $ "Bounding Box (lat, lon): (" ++ (show minlat) ++ "," ++ (show minlon) ++ ") (" ++ (show maxlat) ++ "," ++ (show maxlon) ++ ")"
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Header data"
            processData xs (count + 1) dbMVar
          "OSMData" -> do
            let Right (primitiveBlock, _) = messageGet blobUncompressed :: Either String (PrimitiveBlock, BLAZY.ByteString)
            let st = map U.toString $ F.toList $ getVal (getVal primitiveBlock stringtable) s
            let gran = fromIntegral $ getVal primitiveBlock granularity
            let pg = F.toList $ getVal primitiveBlock primitivegroup
            entryCount <- primitiveGroups pg st gran dbMVar 0
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Entries parsed = [" ++ (show entryCount) ++ "]"
            processData xs (count + 1) dbMVar


      -- Primitive Groups
      primitiveGroups [] _ _ _ count = return count
      primitiveGroups (x:xs) st gran dbMVar count = do 
        let pgNodes = getVal x dense
        let pgWays = getVal x ways
        let pgRelations = getVal x relations

        let impNodes = denseNodes pgNodes
        putMVar dbMVar impNodes
        primitiveGroups xs st gran dbMVar (count + (length impNodes))
        where
          denseNodes :: DenseNodes -> [ImportNode]
          denseNodes d = do 
            let ids = map fromIntegral $ F.toList (getVal d OSM.OSMFormat.DenseNodes.id)
            let latitudes = map fromIntegral $ F.toList (getVal d lat) 
            let longitudes = map fromIntegral $ F.toList (getVal d lon)
            let keyvals = map fromIntegral $ F.toList (getVal d keys_vals)
            let info = getVal d denseinfo
            let versions =  map fromIntegral $ F.toList (getVal info OSM.OSMFormat.DenseInfo.version)
            let timestamps = map fromIntegral $ F.toList (getVal info OSM.OSMFormat.DenseInfo.timestamp) 
            let changesets = map fromIntegral $ F.toList (getVal info OSM.OSMFormat.DenseInfo.changeset)
            let uids = map fromIntegral $ F.toList (getVal info OSM.OSMFormat.DenseInfo.uid)
            let sids = map fromIntegral $ F.toList (getVal info OSM.OSMFormat.DenseInfo.user_sid)
            buildNodeData ids latitudes longitudes keyvals versions timestamps changesets uids sids

          buildNodeData :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [ImportNode]
          buildNodeData ids lat lon keyvals versions timestamps changesets uids sids = do
            let identifiers = deltaDecode ids 0 []
            let latitudes = calculateDegrees (deltaDecode lat 0 []) [] gran
            let longitudes = calculateDegrees (deltaDecode lon 0 []) [] gran
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
            let buildNode = ImportNode {_id=id
                                      , latitude=lat
                                      , longitude=long
                                      , tags=(fst nextKeyVals)
                                      , Data.Node.version=Nothing
                                      , Data.Node.timestamp=Nothing
                                      , Data.Node.changeset=Nothing
                                      , Data.Node.uid=Nothing
                                      , sid=Nothing}
            buildNodes ids lats longs (snd nextKeyVals) [] [] [] [] [] (buildNode : nodes)
            where 
              nextKeyVals = splitKeyVal keyvals []

          buildNodes (id:ids) (lat:lats) (long:longs) keyvals (ver:versions) (ts:timestamps) (cs:changesets) (uid:uids) (sid:sids) nodes = do
            let buildNode = ImportNode {_id=id
                                      , latitude=lat
                                      , longitude=long
                                      , tags=(fst nextKeyVals)
                                      , Data.Node.version=(Just ver)
                                      , Data.Node.timestamp=(Just ts)
                                      , Data.Node.changeset=(Just cs)
                                      , Data.Node.uid=(Just uid)
                                      , sid=(Just (st !! (fromIntegral sid :: Int)))}
            buildNodes ids lats longs (snd nextKeyVals) versions timestamps changesets uids sids (buildNode : nodes)
            where 
              nextKeyVals = splitKeyVal keyvals []

          splitKeyVal :: [Integer] -> [ImportTag] -> ([ImportTag], [Integer])
          splitKeyVal [] [] = ([], [])
          splitKeyVal [] y = (y, [])
          splitKeyVal (x:xx:xs) y 
            | x == 0 = (y, (xx : xs))
            | otherwise = splitKeyVal xs (ImportTag (st !! (fromIntegral x :: Int)) (st !! (fromIntegral xx :: Int)) : y)
          splitKeyVal (x:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0
              
