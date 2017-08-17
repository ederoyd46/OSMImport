 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleContexts #-}
 
module Importer where

import Control.Monad (when)
import Data.Binary.Get (Get, getWord32be, getLazyByteString, runGet, bytesRead)
import Codec.Compression.Zlib (decompress)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as BL (readFile, length, ByteString)
import Data.String.Utils (replace)
import Text.ProtocolBuffers.Basic(ByteString,uToString)
import qualified Data.Foldable as F(toList)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Text.ProtocolBuffers (messageGet,getVal)

import Common(nano,deltaDecode,calculateDegrees)
import qualified Data.Node as N
import Data.Tag
import qualified Data.Way as W
import qualified Data.Relation as R
import qualified Database as MDB (openConnection,closeConnection,saveNodes,saveWays,saveRelation)

import OSM.FileFormat.Blob
import OSM.FileFormat.BlockHeader
import OSM.OSMFormat.HeaderBlock
import OSM.OSMFormat.PrimitiveBlock
import OSM.OSMFormat.HeaderBBox
import OSM.OSMFormat.StringTable
import OSM.OSMFormat.DenseInfo
import OSM.OSMFormat.DenseNodes
import OSM.OSMFormat.PrimitiveGroup
import OSM.OSMFormat.Way
import OSM.OSMFormat.Relation
import OSM.OSMFormat.Info

data Chunk = Chunk {blob_header :: BlockHeader, blob :: Blob} deriving (Show)

getChunks :: Integral a => a -> a -> [Chunk] -> Get [Chunk]
getChunks limit location chunks
  | limit > location = do
    len <- getWord32be
    headerBytes <- getLazyByteString (fromIntegral len)
    let Right (blobHeader,_) = messageGet headerBytes ::  Either String (BlockHeader, ByteString)
    blobData <- getLazyByteString $ fromIntegral $ getVal blobHeader datasize
    let Right (blob',_) = messageGet blobData :: Either String (Blob, ByteString)
    bytesRead' <- bytesRead
    let location' = fromIntegral bytesRead'
    getChunks limit location' ((Chunk blobHeader blob') : chunks)
 | otherwise = return $ reverse chunks


startImport :: String -> String -> String -> IO ()
startImport dbconnection dbname filename = do
  pipe <- MDB.openConnection dbconnection
  let dbNodecommand recs = MDB.saveNodes pipe dbname recs
  let dbWaycommand recs = MDB.saveWays pipe dbname recs
  let dbRelationcommand recs = MDB.saveRelation pipe dbname recs
  performImport filename dbNodecommand dbWaycommand dbRelationcommand
  MDB.closeConnection pipe

performImport :: FilePath -> ([N.ImportNode] -> IO ()) -> ([W.ImportWay] -> IO ()) -> ([R.ImportRelation] -> IO ()) -> IO ()
performImport fileName dbNodecommand dbWaycommand dbRelationcommand = do
  handle <- BL.readFile fileName
  let fileLength = fromIntegral $ BL.length handle
  let chunks = runGet (getChunks fileLength (0 :: Integer) []) handle
  putStrLn $ "File Length : [" ++ (show fileLength) ++ "] Contains: [" ++ (show (length chunks)) ++ "] chunks"
  processData chunks (0 :: Integer)
    where
      processData [] _ = return ()
      processData (x:xs) count = do
        let blobUncompressed = decompress $ getVal (blob x) zlib_data
        let btype = getVal (blob_header x) type'
        case uToString btype of
          "OSMHeader" -> do
            let Right (headerBlock, _) = messageGet blobUncompressed :: Either String (HeaderBlock, BL.ByteString)
            let b = getVal headerBlock bbox
            let minlat = (fromIntegral $ getVal b bottom) / nano
            let minlon = (fromIntegral $ getVal b left) / nano
            let maxlat = (fromIntegral $ getVal b top) / nano
            let maxlon = (fromIntegral $ getVal b right) / nano
            putStrLn $ "Bounding Box (lat, lon): (" ++ (show minlat) ++ "," ++ (show minlon) ++ ") (" ++ (show maxlat) ++ "," ++ (show maxlon) ++ ")"
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Header data"
            processData xs (count + 1)
          "OSMData" -> do
            let Right (primitiveBlock, _) = messageGet blobUncompressed :: Either String (PrimitiveBlock, BL.ByteString)
            let st = map U.toString $ F.toList $ getVal (getVal primitiveBlock stringtable) s
            let gran = fromIntegral $ getVal primitiveBlock granularity
            let pg = F.toList $ getVal primitiveBlock primitivegroup
            entryCount <- primitiveGroups pg st gran 0
            putStrLn $ "Chunk : [" ++ (show count) ++ "] Entries parsed = [" ++ (show entryCount) ++ "]"
            processData xs (count + 1)
          _ -> do
            putStrLn $ "Chunk : [" ++ (show count) ++ "] not valid"
            processData xs (count + 1)

      -- Primitive Groups
      primitiveGroups [] _ _ count = return count
      primitiveGroups (x:xs) st gran count = do
        let pgNodes = getVal x dense
        let pgWays = F.toList $ getVal x ways
        let pgRelations = F.toList $ getVal x relations
        let impNodes = denseNodes pgNodes
        let impWays = parseImpWays pgWays
        let impRelations = parseImpRelations pgRelations

        when ((length impNodes) > 0) (dbNodecommand impNodes)
        when ((length impWays) > 0) (dbWaycommand impWays)
        when ((length impRelations) > 0) (dbRelationcommand impRelations)

        let newCount = count + (length impNodes) + (length impWays) + (length impRelations)
        primitiveGroups xs st gran newCount
        where
          parseImpRelations :: [Relation] -> [R.ImportRelation]
          parseImpRelations [] = []
          parseImpRelations (x:xs) = do
            buildImpRelation x : parseImpRelations xs
            where
              buildImpRelation :: Relation -> R.ImportRelation
              buildImpRelation pgRelation = do
                let id = fromIntegral (getVal pgRelation OSM.OSMFormat.Relation.id)
                let keys = map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.keys)
                let vals = map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.vals)
                let info = (getVal pgRelation OSM.OSMFormat.Relation.info)
                let types = map show $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.types)
                let memids = deltaDecode (map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.memids)) 0
                R.ImportRelation { R._id=id
                            , R.tags=(lookupKeyVals keys vals)
                            , R.version=fromIntegral (getVal info OSM.OSMFormat.Info.version)
                            , R.timestamp=fromIntegral (getVal info OSM.OSMFormat.Info.timestamp)
                            , R.changeset=fromIntegral (getVal info OSM.OSMFormat.Info.changeset)
                            , R.user=(st !! (fromIntegral (getVal info OSM.OSMFormat.Info.user_sid) :: Int))
                            , R.members=buildRelationTags types memids
                          }
                  where
                    buildRelationTags :: [String] -> [Int] -> [ImportTag]
                    buildRelationTags [] [] = []
                    buildRelationTags (x:xs) (y:ys) = ImportTag x (show y) : buildRelationTags xs ys

          parseImpWays :: [Way] -> [W.ImportWay]
          parseImpWays [] = []
          parseImpWays (x:xs) = do
            buildImpWay x : parseImpWays xs
            where
              buildImpWay :: Way -> W.ImportWay
              buildImpWay pgWay = do
                let id = fromIntegral (getVal pgWay OSM.OSMFormat.Way.id)
                let keys = map fromIntegral $ F.toList (getVal pgWay OSM.OSMFormat.Way.keys)
                let vals = map fromIntegral $ F.toList (getVal pgWay OSM.OSMFormat.Way.vals)
                let refs = map fromIntegral $ F.toList (getVal pgWay OSM.OSMFormat.Way.refs)
                let info = (getVal pgWay OSM.OSMFormat.Way.info)
                let deltaRefs = deltaDecode refs 0
                W.ImportWay { W._id=id
                            , W.tags=(lookupKeyVals keys vals)
                            , W.version=fromIntegral (getVal info OSM.OSMFormat.Info.version)
                            , W.timestamp=fromIntegral (getVal info OSM.OSMFormat.Info.timestamp)
                            , W.changeset=fromIntegral (getVal info OSM.OSMFormat.Info.changeset)
                            , W.uid=fromIntegral (getVal info OSM.OSMFormat.Info.uid)
                            , W.user=(st !! (fromIntegral (getVal info OSM.OSMFormat.Info.user_sid) :: Int))
                            , W.nodes=deltaRefs}

          denseNodes :: DenseNodes -> [N.ImportNode]
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

          buildNodeData :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [N.ImportNode]
          buildNodeData ids lat lon keyvals versions timestamps changesets uids sids = do
            let identifiers = deltaDecode ids 0
            let latitudes = calculateDegrees (deltaDecode lat 0) gran
            let longitudes = calculateDegrees (deltaDecode lon 0) gran
            let decodedTimestamps = deltaDecode timestamps 0
            let decodedChangesets = deltaDecode changesets 0
            let decodedUIDs = deltaDecode uids 0
            let decodedUsers = deltaDecode sids 0

            buildNodes identifiers latitudes longitudes keyvals versions decodedTimestamps decodedChangesets decodedUIDs decodedUsers

          buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [N.ImportNode]
          buildNodes [] [] [] [] [] [] [] [] [] = []
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals (ver:versions) (ts:timestamps) (cs:changesets) (uid:uids) (sid:sids) =
                          N.ImportNodeFull {  N._id=id
                                            , N.latitude=lat
                                            , N.longitude=long
                                            , N.tags=(fst $ lookupMixedKeyVals keyvals)
                                            , N.version=ver
                                            , N.timestamp=ts
                                            , N.changeset=cs
                                            , N.uid=uid
                                            , N.sid=(st !! (fromIntegral sid :: Int))
                                          } : buildNodes ids lats longs (snd $ lookupMixedKeyVals keyvals) versions timestamps changesets uids sids
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals [] [] [] [] [] =
                          N.ImportNodeSmall id lat long (fst $ lookupMixedKeyVals keyvals) : buildNodes ids lats longs (snd $ lookupMixedKeyVals keyvals) [] [] [] [] []


          lookupMixedKeyVals :: [Integer] -> ([ImportTag], [Integer])
          lookupMixedKeyVals keyvals= splitKeyVal keyvals []
            where
              splitKeyVal :: [Integer] -> [ImportTag] -> ([ImportTag], [Integer])
              splitKeyVal [] [] = ([], [])
              splitKeyVal [] y = (y, [])
              splitKeyVal (x:xx:xs) y
                | x == 0 = (y, (xx : xs))
                | otherwise = splitKeyVal xs (ImportTag (fixIllegalFieldName $ st !! (fromIntegral x :: Int)) (st !! (fromIntegral xx :: Int)) : y)
              splitKeyVal (_:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0


          lookupKeyVals :: [Int] -> [Int] -> [ImportTag]
          lookupKeyVals [] [] = []
          lookupKeyVals (x:xs) (y:ys) = do
            ImportTag (fixIllegalFieldName $ st !! x) (st !! y) : lookupKeyVals xs ys

          -- Fixes Mongos no . in the field name rule
          fixIllegalFieldName :: String -> String
          fixIllegalFieldName s = replace "." ":" s

showUsage :: IO ()
showUsage = do
      hPutStrLn stderr "usage: dbconnection dbname filename"
      hPutStrLn stderr "example: OSMImport '127.0.0.1:27017' 'geo_data' './download/england-latest.osm.pbf'"
      exitFailure
