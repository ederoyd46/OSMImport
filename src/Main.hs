{-# LANGUAGE OverloadedStrings #-}

import OSMFormat
import Common
import Database
import qualified Data.Serialize as S
import Data.ProtocolBuffers
import Control.Concurrent (forkIO)
import Control.Monad(when)
import GHC.Generics (Generic)
import Data.Maybe(fromJust, isJust, isNothing)
import Data.Binary.Get
import Codec.Compression.Zlib
import System.Exit
import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BCE

main :: IO ()
main =  do 
  args <- getArgs
  when (length args < 3) $ do
      hPutStrLn stderr "usage: dbconnection dbname filename"
      hPutStrLn stderr "example: OSMImport '127.0.0.1:7720' 'geo_data' './download/england-latest.osm.pbf'"
      exitFailure
  
  let dbconnection = args !! 0
  let dbname = args !! 1
  let filename = args !! 2
  
  performImport filename dbconnection dbname
  return ()

data Chunk = Chunk {
    blob_header :: BlobHeader,
    blob :: Blob
} deriving (Show)

getChunks :: Integral a => a -> a -> [Chunk] -> Get [Chunk]
getChunks limit location chunks
  | limit > location = do
    len <- getWord32be
    headerBytes <- getBytes (fromIntegral len)
    let Right blobHeader = S.runGet decodeMessage =<< Right headerBytes :: Either String BlobHeader
    blobData <- getBytes (fromIntegral $ (getField $ bh_datasize blobHeader) :: Int)
    let Right blob = S.runGet decodeMessage =<< Right blobData :: Either String Blob
    bytesRead <- bytesRead
    let location = fromIntegral bytesRead
    getChunks limit location (chunks ++ [Chunk blobHeader blob])
  | otherwise = return chunks

performImport :: FilePath -> [Char] -> [Char] -> IO ()
performImport fileName dbconnection dbname = do
  handle <- BS.readFile fileName
  let fileLength = fromIntegral $ BS.length handle
  let chunks = runGet (getChunks fileLength (0 :: Integer) []) handle
  putStrLn $ "File Length : [" ++ (show fileLength) ++ "] Contains: [" ++ (show (length chunks)) ++ "] chunks"

  processData chunks 1
    where
      processData [] _ = return ()
      processData (x:xs) count = do
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
          "OSMData" -> do
            let eitherDataBlock = S.runGetLazy decodeMessage =<< Right blobUncompressed :: Either String PrimitiveBlock
            case eitherDataBlock of
              Left msg ->
                putStrLn $ "Chunk : [" ++ (show count) ++ "] Not a parsable node [" ++ msg ++ "]"
              Right dataBlock -> do
                let stringTable = Prelude.map BCE.unpack (getField $ st_bytes (getField $ pb_stringtable dataBlock))
                let granularity = fromIntegral $ fromJust . getField $ pb_date_granularity dataBlock
                let pg = getField $ pb_primitivegroup dataBlock
                nodes <- primitiveGroups pg [] stringTable granularity
                saveNodes dbconnection dbname nodes
                putStrLn $ "Chunk : [" ++ (show count) ++ "] Nodes parsed = [" ++ (show (length nodes)) ++ "]"

        processData xs (count + 1)

      -- Primitive Groups
      primitiveGroups :: [PrimitiveGroup] -> [ImportNode] -> [String] -> Integer -> IO [ImportNode]
      primitiveGroups [] [] _ _ = return []
      primitiveGroups [] y _ _ = return y
      primitiveGroups (x:xs) y stringTable granularity = do 
        let s = getField $ pg_dense x
        let impNodes = denseNodes (fromJust s)
        primitiveGroups xs (y ++ impNodes) stringTable granularity
        where
          denseNodes :: DenseNodes -> [ImportNode]
          denseNodes d = do 
            let ids = map fromIntegral (getField $ dense_nodes_id d)
            let latitudes = map fromIntegral (getField $ dense_nodes_lat d)
            let longitudes = map fromIntegral (getField $ dense_nodes_lon d)
            let keyvals = map fromIntegral (getField $ dense_nodes_keys_vals d)
            buildNodeData ids latitudes longitudes keyvals -- This takes a long time to decode the delta values
            -- let info = fromJust $ getField $ dense_nodes_info d

          buildNodeData :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [ImportNode]
          buildNodeData ids lat lon keyvals = do
            let identifiers = deltaDecode ids 0 []
            let latitudes = calculateDegrees (deltaDecode lat 0 []) [] granularity 
            let longitudes = calculateDegrees (deltaDecode lon 0 []) [] granularity 
            buildNodes identifiers latitudes longitudes keyvals []

          buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [ImportNode] -> [ImportNode]
          buildNodes [] [] [] [] [] = []
          buildNodes [] [] [] [] nodes = nodes
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals nodes = do
            let buildNode = ImportNode {_id=id, latitude=lat, longitude=long, tags=(fst nextKeyVals)}
            buildNodes ids lats longs (snd nextKeyVals) (buildNode : nodes)
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







