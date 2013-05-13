{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeOperators #-}

import OSMFormat
import Dto
import Database
import qualified Data.Serialize as S
import Data.Int
import Data.ProtocolBuffers
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.State(liftIO)
import Control.Monad(when)
import Data.TypeLevel (D1, D2, D3)
import GHC.Generics (Generic)
import Data.Maybe(fromJust, isJust, isNothing)
import Data.Binary.Get
import Data.Tuple
import Codec.Compression.Zlib
import System.Exit
import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BCE

nano :: Float
nano = 1000000000

data ParserHeader = ParserHeader {
    parser_blob_header :: BlobHeader,
    parser_blob :: Blob,
    parser_offset :: Int64
}

getHeaderChunk :: Int -> Get (ParserHeader)
getHeaderChunk offset = do  
  _ <- skip offset
  len <- getWord32be
  headerBytes <- getBytes (fromIntegral len)
  let Right blobHeader = S.runGet decodeMessage =<< Right headerBytes :: Either String BlobHeader
  blobData <- getBytes (fromIntegral $ (getField $ bh_datasize blobHeader) :: Int)
  let Right blob = S.runGet decodeMessage =<< Right blobData :: Either String Blob
  -- let headerRawSize = (getField $ b_raw_size blob)
  -- headerBlockBytes <- getBytes (fromIntegral (fromJust headerRawSize))
  -- let Right headerBlock = S.runGet decodeMessage =<< unhex (hex headerBlockBytes) :: Either String HeaderBlock
  bytesRead <- bytesRead
  return $ ParserHeader blobHeader blob bytesRead

loopFile :: String -> String -> String -> Int -> IO (Int)
loopFile dbconnection dbname filename offset = do 
  input <- BS.readFile filename
  let header = runGet (getHeaderChunk offset) input
  print $ getField $ bh_type (parser_blob_header header)

  let blobCompressed = fromJust $ getField $ b_zlib_data (parser_blob header)
  let blobData = BCE.concat $ BS.toChunks . decompress . BS.fromChunks $ [blobCompressed]
  let btype = getField $ bh_type (parser_blob_header header)
  case btype of
    "OSMHeader" -> do 
      let Right headerBlock = S.runGet decodeMessage =<< Right blobData :: Either String HeaderBlock
      let features = getField $ hb_required_features headerBlock
      let bbox = fromJust $ getField $ hb_bbox headerBlock
      let minlat = (fromIntegral $ getField $ hbbox_bottom bbox) / nano
      let minlon = (fromIntegral $ getField $ hbbox_left bbox) / nano
      let maxlat = (fromIntegral $ getField $ hbbox_top bbox) / nano
      let maxlon = (fromIntegral $ getField $ hbbox_right bbox) / nano
      putStrLn $ "Bounding Box (lat, lon): (" ++ (show minlat) ++ "," ++ (show maxlat) ++ ") (" ++ (show minlon) ++ "," ++ (show maxlon) ++ ")"
      print features
    "OSMData" -> do
      let Right dataBlock = S.runGet decodeMessage =<< Right blobData :: Either String PrimitiveBlock
      let stringTable = Prelude.map BCE.unpack (getField $ st_bytes (getField $ pb_stringtable dataBlock))
      putStrLn $ "lat_offset: " ++ (show . getField $ pb_lat_offset dataBlock)
      putStrLn $ "lon_offset: " ++ (show . getField $ pb_lon_offset dataBlock)
      putStrLn $ "granularity: " ++ (show . fromJust . getField $ pb_granularity dataBlock)
      -- putStrLn $ "pb_date_granularity: " ++ (show . fromJust . getField $ pb_date_granularity dataBlock)
      let pg = getField $ pb_primitivegroup dataBlock
      nodes <- primitiveGroups pg [] stringTable
      saveNodes dbconnection dbname nodes
      -- thread <- forkIO $ saveNodes nodes
      -- print thread

  print $ "Offset: " ++ (show $ parser_offset header)
  loopFile dbconnection dbname filename (fromIntegral (parser_offset header))
  -- return 138 -- This is the size of the header for UK

-- Primitive Groups
primitiveGroups :: [PrimitiveGroup] -> [ImportNode] -> [String] -> IO [ImportNode]
primitiveGroups [] [] _ = return []
primitiveGroups [] y _ = return y
primitiveGroups (x:xs) y stringTable = do 
  let s = getField $ pg_dense x
  let impNodes = denseNodes (fromJust s)
  primitiveGroups xs (y ++ impNodes) stringTable
  where
    denseNodes :: DenseNodes -> [ImportNode]
    denseNodes d = do 
      let ids = Prelude.map fromIntegral (getField $ dense_nodes_id d)
      let latitudes = Prelude.map fromIntegral (getField $ dense_nodes_lat d)
      let longitudes = Prelude.map fromIntegral (getField $ dense_nodes_lon d)
      let keyvals = Prelude.map fromIntegral (getField $ dense_nodes_keys_vals d)
      buildNodeData ids latitudes longitudes keyvals

    buildNodeData :: [Integer] -> [Float] -> [Float] -> [Integer] -> [ImportNode]
    buildNodeData ids lat lon keyvals= 
      buildNodes (deltaDecode ids 0 []) (calculateDegrees lat [] 100 0) (calculateDegrees lon [] 100 0) keyvals []

    buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [ImportNode] -> [ImportNode]
    buildNodes [] [] [] [] [] = []
    buildNodes [] [] [] [] nodes = nodes
    buildNodes (x:ids) (y:lats) (z:longs) keyvals nodes = do
      buildNodes ids lats longs (snd nextKeyVals) (nodes ++ [ImportNode x y z (fst nextKeyVals)])
      where
        nextKeyVals = splitKeyVal keyvals []

    deltaDecode :: Num a => [a] -> a -> [a] -> [a]
    deltaDecode [] _ [] = []
    deltaDecode [] _ rest = rest
    deltaDecode (x:xs) offset rest = do
      let lastId = offset + x
      deltaDecode xs lastId (rest ++ [lastId])

    calculateDegrees :: [Float] -> [Float] -> Float -> Float -> [Float]
    calculateDegrees [] [] gran lastlat = []
    calculateDegrees [] y gran lastlat = y
    calculateDegrees (x:xs) y gran lastlat = do
      let newlastlat = lastlat + x
      let newcoordinate = (newlastlat * gran) / nano
      calculateDegrees xs (y ++ [newcoordinate]) gran (newlastlat)

    splitKeyVal :: [Integer] -> [ParseTag] -> ([ParseTag], [Integer])
    splitKeyVal [] [] = ([], [])
    splitKeyVal [] y = (y, [])
    splitKeyVal (x:xx:xs) y = 
      if (x == 0)
        then (y, ([xx] ++ xs))
        else splitKeyVal xs (y ++ [ParseTag (stringTable !! (fromInteger x)) (stringTable !! (fromInteger xx))])
    splitKeyVal (x:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0


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
  
  loopFile dbconnection dbname filename 0
  return ()

  
