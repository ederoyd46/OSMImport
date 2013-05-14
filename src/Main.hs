{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeOperators #-}

import OSMFormat
import Common
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
        let blobData = BCE.concat $ BS.toChunks . decompress . BS.fromChunks $ [blobCompressed]
        let btype = getField $ bh_type (blob_header x)
        case btype of
          "OSMHeader" -> do 
            let Right headerBlock = S.runGet decodeMessage =<< Right blobData :: Either String HeaderBlock
            -- let features = getField $ hb_required_features headerBlock
            let bbox = fromJust $ getField $ hb_bbox headerBlock
            let minlat = (fromIntegral $ getField $ hbbox_bottom bbox) / nano
            let minlon = (fromIntegral $ getField $ hbbox_left bbox) / nano
            let maxlat = (fromIntegral $ getField $ hbbox_top bbox) / nano
            let maxlon = (fromIntegral $ getField $ hbbox_right bbox) / nano
            putStrLn $ "Bounding Box (lat, lon): (" ++ (show minlat) ++ "," ++ (show maxlat) ++ ") (" ++ (show minlon) ++ "," ++ (show maxlon) ++ ")"
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Header data"
          "OSMData" -> do
            let Right dataBlock = S.runGet decodeMessage =<< Right blobData :: Either String PrimitiveBlock
            let stringTable = Prelude.map BCE.unpack (getField $ st_bytes (getField $ pb_stringtable dataBlock))
            let granularity = fromIntegral $ fromJust . getField $ pb_date_granularity dataBlock
            let pg = getField $ pb_primitivegroup dataBlock
            nodes <- primitiveGroups pg [] stringTable granularity
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Nodes parsed = [" ++ (show (length nodes)) ++ "]"
            saveNodes dbconnection dbname nodes
        
        processData xs (count + 1)

      -- Primitive Groups
      primitiveGroups :: [PrimitiveGroup] -> [ImportNode] -> [String] -> Float -> IO [ImportNode]
      primitiveGroups [] [] _ _ = return []
      primitiveGroups [] y _ _ = return y
      primitiveGroups (x:xs) y stringTable granularity = do 
        let s = getField $ pg_dense x
        let impNodes = denseNodes (fromJust s)
        primitiveGroups xs (y ++ impNodes) stringTable granularity
        where
          denseNodes :: DenseNodes -> [ImportNode]
          denseNodes d = do 
            let ids = Prelude.map fromIntegral (getField $ dense_nodes_id d)
            let latitudes = Prelude.map fromIntegral (getField $ dense_nodes_lat d)
            let longitudes = Prelude.map fromIntegral (getField $ dense_nodes_lon d)
            let keyvals = Prelude.map fromIntegral (getField $ dense_nodes_keys_vals d)
            let info = fromJust $ getField $ dense_nodes_info d
            buildNodeData ids latitudes longitudes keyvals
            -- augmentWithDenseInfo nodes (dense_info_changeset info) (dense_info_uid info) (dense_info_version info) (dense_info_user_sid info) (dense_info_time info) []
          -- augmentWithDenseInfo (node:nodes) (changeset:changesets) (uid:uids) (version:versions) (info:infos) (user:users) (time:times) y = do
          --   -- let augment = node {changeset=changeset, uid=uid, version=version, user=user, time=time}
          --   augmentWithDenseInfo nodes changesets uids version infos users times y

          buildNodeData :: [Integer] -> [Float] -> [Float] -> [Integer] -> [ImportNode]
          buildNodeData ids lat lon keyvals = 
            buildNodes (deltaDecode ids 0 []) (calculateDegrees lat [] granularity 0) (calculateDegrees lon [] granularity 0) keyvals []

          buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [ImportNode] -> [ImportNode]
          buildNodes [] [] [] [] [] = []
          buildNodes [] [] [] [] nodes = nodes
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals nodes = do
            let buildNode = ImportNode {_id=id, latitude=lat, longitude=long, tags=(fst nextKeyVals)}
            buildNodes ids lats longs (snd nextKeyVals) (nodes ++ [buildNode])
            where
              nextKeyVals = splitKeyVal keyvals []

          calculateDegrees :: [Float] -> [Float] -> Float -> Float -> [Float]
          calculateDegrees [] [] gran lastlat = []
          calculateDegrees [] y gran lastlat = y
          calculateDegrees (x:xs) y gran lastlat = do
            let newlastlat = lastlat + x
            let newcoordinate = (newlastlat * gran) / nano
            calculateDegrees xs (y ++ [newcoordinate]) gran (newlastlat)

          splitKeyVal :: [Integer] -> [ImportTag] -> ([ImportTag], [Integer])
          splitKeyVal [] [] = ([], [])
          splitKeyVal [] y = (y, [])
          splitKeyVal (x:xx:xs) y = 
            if (x == 0)
              then (y, ([xx] ++ xs))
              else splitKeyVal xs (y ++ [ImportTag (stringTable !! (fromInteger x)) (stringTable !! (fromInteger xx))])
          splitKeyVal (x:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0
