{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module OSM.OSMFormat.ChangeSet (ChangeSet(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.HeaderBBox as OSM.OSMFormat (HeaderBBox)
import qualified OSM.OSMFormat.Info as OSM.OSMFormat (Info)
 
data ChangeSet = ChangeSet{id :: !P'.Int64, keys :: !(P'.Seq P'.Word32), vals :: !(P'.Seq P'.Word32),
                           info :: !(P'.Maybe OSM.OSMFormat.Info), created_at :: !P'.Int64, closetime_delta :: !(P'.Maybe P'.Int64),
                           open :: !P'.Bool, bbox :: !(P'.Maybe OSM.OSMFormat.HeaderBBox), unknown'field :: !P'.UnknownField}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage ChangeSet where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable ChangeSet where
  mergeAppend (ChangeSet x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (ChangeSet y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = ChangeSet (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
 
instance P'.Default ChangeSet where
  defaultValue
   = ChangeSet P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire ChangeSet where
  wireSize ft' self'@(ChangeSet x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 3 x'1 + P'.wireSizePacked 1 13 x'2 + P'.wireSizePacked 1 13 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeReq 1 3 x'5
             + P'.wireSizeOpt 1 3 x'6
             + P'.wireSizeReq 1 8 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeUnknownField x'9)
  wirePut ft' self'@(ChangeSet x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 3 x'1
             P'.wirePutPacked 18 13 x'2
             P'.wirePutPacked 26 13 x'3
             P'.wirePutOpt 34 11 x'4
             P'.wirePutReq 64 3 x'5
             P'.wirePutOpt 72 3 x'6
             P'.wirePutReq 80 8 x'7
             P'.wirePutOpt 90 11 x'8
             P'.wirePutUnknownField x'9
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = new'Field}) (P'.wireGet 3)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{keys = P'.append (keys old'Self) new'Field}) (P'.wireGet 13)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{keys = P'.mergeAppend (keys old'Self) new'Field}) (P'.wireGetPacked 13)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{vals = P'.append (vals old'Self) new'Field}) (P'.wireGet 13)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{vals = P'.mergeAppend (vals old'Self) new'Field}) (P'.wireGetPacked 13)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{info = P'.mergeAppend (info old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{created_at = new'Field}) (P'.wireGet 3)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{closetime_delta = Prelude'.Just new'Field}) (P'.wireGet 3)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{open = new'Field}) (P'.wireGet 8)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{bbox = P'.mergeAppend (bbox old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ChangeSet) ChangeSet where
  getVal m' f' = f' m'
 
instance P'.GPB ChangeSet
 
instance P'.ReflectDescriptor ChangeSet where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [8, 64, 80]) (P'.fromDistinctAscList [8, 16, 18, 24, 26, 34, 64, 72, 80, 90])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.ChangeSet\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"ChangeSet\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"ChangeSet.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.id\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"id\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.keys\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"keys\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.vals\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"vals\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Just (WireTag {getWireTag = 24},WireTag {getWireTag = 26}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.info\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"info\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Info\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Info\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.created_at\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"created_at\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 64}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.closetime_delta\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"closetime_delta\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.open\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"open\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.ChangeSet.bbox\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"ChangeSet\"], baseName' = FName \"bbox\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.HeaderBBox\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"HeaderBBox\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"