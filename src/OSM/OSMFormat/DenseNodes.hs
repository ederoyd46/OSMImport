{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module OSM.OSMFormat.DenseNodes (DenseNodes(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.DenseInfo as OSM.OSMFormat (DenseInfo)
 
data DenseNodes = DenseNodes{id :: !(P'.Seq P'.Int64), denseinfo :: !(P'.Maybe OSM.OSMFormat.DenseInfo), lat :: !(P'.Seq P'.Int64),
                             lon :: !(P'.Seq P'.Int64), keys_vals :: !(P'.Seq P'.Int32), unknown'field :: !P'.UnknownField}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage DenseNodes where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable DenseNodes where
  mergeAppend (DenseNodes x'1 x'2 x'3 x'4 x'5 x'6) (DenseNodes y'1 y'2 y'3 y'4 y'5 y'6)
   = DenseNodes (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default DenseNodes where
  defaultValue = DenseNodes P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire DenseNodes where
  wireSize ft' self'@(DenseNodes x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizePacked 1 18 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizePacked 1 18 x'3 + P'.wireSizePacked 1 18 x'4 +
             P'.wireSizePacked 1 5 x'5
             + P'.wireSizeUnknownField x'6)
  wirePut ft' self'@(DenseNodes x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutPacked 10 18 x'1
             P'.wirePutOpt 42 11 x'2
             P'.wirePutPacked 66 18 x'3
             P'.wirePutPacked 74 18 x'4
             P'.wirePutPacked 82 5 x'5
             P'.wirePutUnknownField x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = P'.append (id old'Self) new'Field}) (P'.wireGet 18)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{id = P'.mergeAppend (id old'Self) new'Field}) (P'.wireGetPacked 18)
             42 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{denseinfo = P'.mergeAppend (denseinfo old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{lat = P'.append (lat old'Self) new'Field}) (P'.wireGet 18)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{lat = P'.mergeAppend (lat old'Self) new'Field}) (P'.wireGetPacked 18)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{lon = P'.append (lon old'Self) new'Field}) (P'.wireGet 18)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{lon = P'.mergeAppend (lon old'Self) new'Field}) (P'.wireGetPacked 18)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{keys_vals = P'.append (keys_vals old'Self) new'Field}) (P'.wireGet 5)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{keys_vals = P'.mergeAppend (keys_vals old'Self) new'Field})
                    (P'.wireGetPacked 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> DenseNodes) DenseNodes where
  getVal m' f' = f' m'
 
instance P'.GPB DenseNodes
 
instance P'.ReflectDescriptor DenseNodes where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 10, 42, 64, 66, 72, 74, 80, 82])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.DenseNodes\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"DenseNodes\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"DenseNodes.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseNodes.id\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseNodes\"], baseName' = FName \"id\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Just (WireTag {getWireTag = 8},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseNodes.denseinfo\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseNodes\"], baseName' = FName \"denseinfo\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.DenseInfo\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"DenseInfo\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseNodes.lat\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseNodes\"], baseName' = FName \"lat\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Just (WireTag {getWireTag = 64},WireTag {getWireTag = 66}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseNodes.lon\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseNodes\"], baseName' = FName \"lon\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Just (WireTag {getWireTag = 72},WireTag {getWireTag = 74}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseNodes.keys_vals\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseNodes\"], baseName' = FName \"keys_vals\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Just (WireTag {getWireTag = 80},WireTag {getWireTag = 82}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"