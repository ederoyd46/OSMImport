 {-# LANGUAGE OverloadedStrings #-}
 {-# LANGUAGE FlexibleContexts #-}

module Importer
  (performImport)
where

import Control.Monad (when)
import Data.Binary.Get (Get, getWord32be, getLazyByteString, runGet, bytesRead)
import Codec.Compression.Zlib (decompress)
import qualified Data.ByteString.Lazy as BL (readFile, length, ByteString)
import Text.ProtocolBuffers.Basic(ByteString,uToString)
import qualified Data.Foldable as F(toList)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Text.ProtocolBuffers (messageGet,getVal)

import Common(nano,deltaDecode,calculateDegrees)
import Types

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

performImport :: FilePath -> ([ImportNode] -> IO ()) -> ([ImportWay] -> IO ()) -> ([ImportRelation] -> IO ()) -> IO ()
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
            let st = map U.toString . F.toList $ getVal (getVal primitiveBlock stringtable) s
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
          parseImpRelations :: [Relation] -> [ImportRelation]
          parseImpRelations [] = []
          parseImpRelations (x:xs) = do
            buildImpRelation x : parseImpRelations xs
            where
              buildImpRelation :: Relation -> ImportRelation
              buildImpRelation pgRelation = do
                let id = toInteger (getVal pgRelation OSM.OSMFormat.Relation.id)
                let keys = map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.keys)
                let vals = map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.vals)
                let info = (getVal pgRelation OSM.OSMFormat.Relation.info)
                let types = map show $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.types)
                let memids = deltaDecode (map fromIntegral $ F.toList (getVal pgRelation OSM.OSMFormat.Relation.memids))
                ImportRelation id
                              (lookupKeyVals keys vals)
                              (toInteger (getVal info OSM.OSMFormat.Info.version))
                              (toInteger (getVal info OSM.OSMFormat.Info.timestamp))
                              (toInteger (getVal info OSM.OSMFormat.Info.changeset))
                              (st !! (fromIntegral (getVal info OSM.OSMFormat.Info.user_sid) :: Int))
                              (buildRelationTags types memids)

                  where
                    buildRelationTags :: [String] -> [Int] -> [ImportTag]
                    buildRelationTags [] [] = []
                    buildRelationTags (x:xs) (y:ys) = ImportTag x (show y) : buildRelationTags xs ys

          parseImpWays :: [Way] -> [ImportWay]
          parseImpWays [] = []
          parseImpWays (x:xs) = do
            buildImpWay x : parseImpWays xs
            where
              buildImpWay :: Way -> ImportWay
              buildImpWay pgWay = do
                let id = toInteger (getVal pgWay OSM.OSMFormat.Way.id)
                let keys = map fromIntegral $ F.toList (getVal pgWay OSM.OSMFormat.Way.keys)
                let vals = map fromIntegral $ F.toList (getVal pgWay OSM.OSMFormat.Way.vals)
                let refs = map toInteger $ F.toList (getVal pgWay OSM.OSMFormat.Way.refs)
                let info = (getVal pgWay OSM.OSMFormat.Way.info)
                let deltaRefs = deltaDecode refs
                ImportWay id
                          (lookupKeyVals keys vals)
                          (toInteger (getVal info OSM.OSMFormat.Info.version))
                          (toInteger (getVal info OSM.OSMFormat.Info.timestamp))
                          (toInteger (getVal info OSM.OSMFormat.Info.changeset))
                          (toInteger (getVal info OSM.OSMFormat.Info.uid))
                          (st !! (fromIntegral (getVal info OSM.OSMFormat.Info.user_sid) :: Int))
                          deltaRefs

          denseNodes :: DenseNodes -> [ImportNode]
          denseNodes d = do
            let ids = map toInteger $ F.toList (getVal d OSM.OSMFormat.DenseNodes.id)
            let latitudes = map toInteger $ F.toList (getVal d lat)
            let longitudes = map toInteger $ F.toList (getVal d lon)
            let keyvals = map toInteger $ F.toList (getVal d keys_vals)
            let info = getVal d denseinfo
            let versions =  map toInteger $ F.toList (getVal info OSM.OSMFormat.DenseInfo.version)
            let timestamps = map toInteger $ F.toList (getVal info OSM.OSMFormat.DenseInfo.timestamp)
            let changesets = map toInteger $ F.toList (getVal info OSM.OSMFormat.DenseInfo.changeset)
            let uids = map toInteger $ F.toList (getVal info OSM.OSMFormat.DenseInfo.uid)
            let sids = map toInteger $ F.toList (getVal info OSM.OSMFormat.DenseInfo.user_sid)
            buildNodeData ids latitudes longitudes keyvals versions timestamps changesets uids sids

          buildNodeData :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [ImportNode]
          buildNodeData ids lat lon keyvals versions timestamps changesets uids sids = do
            let identifiers = deltaDecode ids
            let latitudes = calculateDegrees (deltaDecode lat) gran
            let longitudes = calculateDegrees (deltaDecode lon) gran
            let decodedTimestamps = deltaDecode timestamps
            let decodedChangesets = deltaDecode changesets
            let decodedUIDs = deltaDecode uids
            let decodedUsers = deltaDecode sids

            buildNodes identifiers latitudes longitudes keyvals versions decodedTimestamps decodedChangesets decodedUIDs decodedUsers

          buildNodes :: [Integer] -> [Float] -> [Float] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [ImportNode]
          buildNodes [] [] [] [] [] [] [] [] [] = []
          buildNodes (id:ids) (lat:lats) (long:longs) keyvals (ver:versions) (ts:timestamps) (cs:changesets) (uid:uids) (sid:sids) =
                          ImportNode id
                                     lat
                                     long
                                     (fst $ lookupMixedKeyVals keyvals)
                                     ver
                                     ts
                                     cs
                                     uid
                                     (st !! (fromIntegral sid :: Int))
                                    : buildNodes ids lats longs (snd $ lookupMixedKeyVals keyvals) versions timestamps changesets uids sids

          lookupMixedKeyVals :: [Integer] -> ([ImportTag], [Integer])
          lookupMixedKeyVals keyvals= splitKeyVal keyvals []
            where
              splitKeyVal :: [Integer] -> [ImportTag] -> ([ImportTag], [Integer])
              splitKeyVal [] [] = ([], [])
              splitKeyVal [] y = (y, [])
              splitKeyVal (x:xx:xs) y
                | x == 0 = (y, (xx : xs))
                | otherwise = splitKeyVal xs (ImportTag (st !! (fromIntegral x :: Int)) (st !! (fromIntegral xx :: Int)) : y)
              splitKeyVal (_:_) y = (y, []) -- In the case that the array is on an unequal number, Can happen if the last couple of entries are 0


          lookupKeyVals :: [Int] -> [Int] -> [ImportTag]
          lookupKeyVals [] [] = []
          lookupKeyVals (x:xs) (y:ys) = do
            ImportTag (st !! x) (st !! y) : lookupKeyVals xs ys

