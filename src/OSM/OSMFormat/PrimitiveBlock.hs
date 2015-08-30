{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module OSM.OSMFormat.PrimitiveBlock (PrimitiveBlock(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.PrimitiveGroup as OSM.OSMFormat (PrimitiveGroup)
import qualified OSM.OSMFormat.StringTable as OSM.OSMFormat (StringTable)
 
data PrimitiveBlock = PrimitiveBlock{stringtable :: !(OSM.OSMFormat.StringTable),
                                     primitivegroup :: !(P'.Seq OSM.OSMFormat.PrimitiveGroup), granularity :: !(P'.Maybe P'.Int32),
                                     lat_offset :: !(P'.Maybe P'.Int64), lon_offset :: !(P'.Maybe P'.Int64),
                                     date_granularity :: !(P'.Maybe P'.Int32), unknown'field :: !(P'.UnknownField)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage PrimitiveBlock where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable PrimitiveBlock where
  mergeAppend (PrimitiveBlock x'1 x'2 x'3 x'4 x'5 x'6 x'7) (PrimitiveBlock y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = PrimitiveBlock (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
 
instance P'.Default PrimitiveBlock where
  defaultValue
   = PrimitiveBlock P'.defaultValue P'.defaultValue (Prelude'.Just 100) (Prelude'.Just 0) (Prelude'.Just 0) (Prelude'.Just 1000)
      P'.defaultValue
 
instance P'.Wire PrimitiveBlock where
  wireSize ft' self'@(PrimitiveBlock x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeOpt 2 5 x'3 + P'.wireSizeOpt 2 3 x'4 +
             P'.wireSizeOpt 2 3 x'5
             + P'.wireSizeOpt 2 5 x'6
             + P'.wireSizeUnknownField x'7)
  wirePut ft' self'@(PrimitiveBlock x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutRep 18 11 x'2
             P'.wirePutOpt 136 5 x'3
             P'.wirePutOpt 144 5 x'6
             P'.wirePutOpt 152 3 x'4
             P'.wirePutOpt 160 3 x'5
             P'.wirePutUnknownField x'7
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{stringtable = P'.mergeAppend (stringtable old'Self) (new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{primitivegroup = P'.append (primitivegroup old'Self) new'Field})
                    (P'.wireGet 11)
             136 -> Prelude'.fmap (\ !new'Field -> old'Self{granularity = Prelude'.Just new'Field}) (P'.wireGet 5)
             152 -> Prelude'.fmap (\ !new'Field -> old'Self{lat_offset = Prelude'.Just new'Field}) (P'.wireGet 3)
             160 -> Prelude'.fmap (\ !new'Field -> old'Self{lon_offset = Prelude'.Just new'Field}) (P'.wireGet 3)
             144 -> Prelude'.fmap (\ !new'Field -> old'Self{date_granularity = Prelude'.Just new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> PrimitiveBlock) PrimitiveBlock where
  getVal m' f' = f' m'
 
instance P'.GPB PrimitiveBlock
 
instance P'.ReflectDescriptor PrimitiveBlock where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 18, 136, 144, 152, 160])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.PrimitiveBlock\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"PrimitiveBlock\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"PrimitiveBlock.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveBlock.stringtable\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveBlock\"], baseName' = FName \"stringtable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.StringTable\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"StringTable\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveBlock.primitivegroup\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveBlock\"], baseName' = FName \"primitivegroup\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.PrimitiveGroup\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"PrimitiveGroup\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveBlock.granularity\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveBlock\"], baseName' = FName \"granularity\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Just \"100\", hsDefault = Just (HsDef'Integer 100)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveBlock.lat_offset\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveBlock\"], baseName' = FName \"lat_offset\"}, fieldNumber = FieldId {getFieldId = 19}, wireTag = WireTag {getWireTag = 152}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Just \"0\", hsDefault = Just (HsDef'Integer 0)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveBlock.lon_offset\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveBlock\"], baseName' = FName \"lon_offset\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Just \"0\", hsDefault = Just (HsDef'Integer 0)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveBlock.date_granularity\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveBlock\"], baseName' = FName \"date_granularity\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Just \"1000\", hsDefault = Just (HsDef'Integer 1000)}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"
 
instance P'.TextType PrimitiveBlock where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg PrimitiveBlock where
  textPut msg
   = do
       P'.tellT "stringtable" (stringtable msg)
       P'.tellT "primitivegroup" (primitivegroup msg)
       P'.tellT "granularity" (granularity msg)
       P'.tellT "lat_offset" (lat_offset msg)
       P'.tellT "lon_offset" (lon_offset msg)
       P'.tellT "date_granularity" (date_granularity msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'stringtable, parse'primitivegroup, parse'granularity, parse'lat_offset, parse'lon_offset,
                   parse'date_granularity])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'stringtable
         = P'.try
            (do
               v <- P'.getT "stringtable"
               Prelude'.return (\ o -> o{stringtable = v}))
        parse'primitivegroup
         = P'.try
            (do
               v <- P'.getT "primitivegroup"
               Prelude'.return (\ o -> o{primitivegroup = P'.append (primitivegroup o) v}))
        parse'granularity
         = P'.try
            (do
               v <- P'.getT "granularity"
               Prelude'.return (\ o -> o{granularity = v}))
        parse'lat_offset
         = P'.try
            (do
               v <- P'.getT "lat_offset"
               Prelude'.return (\ o -> o{lat_offset = v}))
        parse'lon_offset
         = P'.try
            (do
               v <- P'.getT "lon_offset"
               Prelude'.return (\ o -> o{lon_offset = v}))
        parse'date_granularity
         = P'.try
            (do
               v <- P'.getT "date_granularity"
               Prelude'.return (\ o -> o{date_granularity = v}))