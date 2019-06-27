{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module OSM.OSMFormat.HeaderBlock (HeaderBlock(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.HeaderBBox as OSM.OSMFormat (HeaderBBox)

data HeaderBlock = HeaderBlock{bbox :: !(P'.Maybe OSM.OSMFormat.HeaderBBox), required_features :: !(P'.Seq P'.Utf8),
                               optional_features :: !(P'.Seq P'.Utf8), writingprogram :: !(P'.Maybe P'.Utf8),
                               source :: !(P'.Maybe P'.Utf8), osmosis_replication_timestamp :: !(P'.Maybe P'.Int64),
                               osmosis_replication_sequence_number :: !(P'.Maybe P'.Int64),
                               osmosis_replication_base_url :: !(P'.Maybe P'.Utf8), unknown'field :: !(P'.UnknownField)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage HeaderBlock where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable HeaderBlock where
  mergeAppend (HeaderBlock x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (HeaderBlock y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = HeaderBlock (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)

instance P'.Default HeaderBlock where
  defaultValue
   = HeaderBlock P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire HeaderBlock where
  wireSize ft' self'@(HeaderBlock x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeRep 1 9 x'2 + P'.wireSizeRep 1 9 x'3 + P'.wireSizeOpt 2 9 x'4 +
             P'.wireSizeOpt 2 9 x'5
             + P'.wireSizeOpt 2 3 x'6
             + P'.wireSizeOpt 2 3 x'7
             + P'.wireSizeOpt 2 9 x'8
             + P'.wireSizeUnknownField x'9)
  wirePutWithSize ft' self'@(HeaderBlock x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 11 x'1, P'.wirePutRepWithSize 34 9 x'2, P'.wirePutRepWithSize 42 9 x'3,
             P'.wirePutOptWithSize 130 9 x'4, P'.wirePutOptWithSize 138 9 x'5, P'.wirePutOptWithSize 256 3 x'6,
             P'.wirePutOptWithSize 264 3 x'7, P'.wirePutOptWithSize 274 9 x'8, P'.wirePutUnknownFieldWithSize x'9]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown' P'.loadUnknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown' P'.loadUnknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{bbox = P'.mergeAppend (bbox old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{required_features = P'.append (required_features old'Self) new'Field})
                    (P'.wireGet 9)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{optional_features = P'.append (optional_features old'Self) new'Field})
                    (P'.wireGet 9)
             130 -> Prelude'.fmap (\ !new'Field -> old'Self{writingprogram = Prelude'.Just new'Field}) (P'.wireGet 9)
             138 -> Prelude'.fmap (\ !new'Field -> old'Self{source = Prelude'.Just new'Field}) (P'.wireGet 9)
             256 -> Prelude'.fmap (\ !new'Field -> old'Self{osmosis_replication_timestamp = Prelude'.Just new'Field}) (P'.wireGet 3)
             264 -> Prelude'.fmap (\ !new'Field -> old'Self{osmosis_replication_sequence_number = Prelude'.Just new'Field})
                     (P'.wireGet 3)
             274 -> Prelude'.fmap (\ !new'Field -> old'Self{osmosis_replication_base_url = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> HeaderBlock) HeaderBlock where
  getVal m' f' = f' m'

instance P'.GPB HeaderBlock

instance P'.ReflectDescriptor HeaderBlock where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 34, 42, 130, 138, 256, 264, 274])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.HeaderBlock\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"HeaderBlock\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"HeaderBlock.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.bbox\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"bbox\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.HeaderBBox\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"HeaderBBox\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.required_features\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"required_features\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.optional_features\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"optional_features\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.writingprogram\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"writingprogram\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 130}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.source\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"source\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 138}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.osmosis_replication_timestamp\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"osmosis_replication_timestamp\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 32}, wireTag = WireTag {getWireTag = 256}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.osmosis_replication_sequence_number\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"osmosis_replication_sequence_number\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 33}, wireTag = WireTag {getWireTag = 264}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBlock.osmosis_replication_base_url\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBlock\"], baseName' = FName \"osmosis_replication_base_url\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 34}, wireTag = WireTag {getWireTag = 274}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType HeaderBlock where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg HeaderBlock where
  textPut msg
   = do
       P'.tellT "bbox" (bbox msg)
       P'.tellT "required_features" (required_features msg)
       P'.tellT "optional_features" (optional_features msg)
       P'.tellT "writingprogram" (writingprogram msg)
       P'.tellT "source" (source msg)
       P'.tellT "osmosis_replication_timestamp" (osmosis_replication_timestamp msg)
       P'.tellT "osmosis_replication_sequence_number" (osmosis_replication_sequence_number msg)
       P'.tellT "osmosis_replication_base_url" (osmosis_replication_base_url msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'bbox, parse'required_features, parse'optional_features, parse'writingprogram, parse'source,
                   parse'osmosis_replication_timestamp, parse'osmosis_replication_sequence_number,
                   parse'osmosis_replication_base_url])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'bbox
         = P'.try
            (do
               v <- P'.getT "bbox"
               Prelude'.return (\ o -> o{bbox = v}))
        parse'required_features
         = P'.try
            (do
               v <- P'.getT "required_features"
               Prelude'.return (\ o -> o{required_features = P'.append (required_features o) v}))
        parse'optional_features
         = P'.try
            (do
               v <- P'.getT "optional_features"
               Prelude'.return (\ o -> o{optional_features = P'.append (optional_features o) v}))
        parse'writingprogram
         = P'.try
            (do
               v <- P'.getT "writingprogram"
               Prelude'.return (\ o -> o{writingprogram = v}))
        parse'source
         = P'.try
            (do
               v <- P'.getT "source"
               Prelude'.return (\ o -> o{source = v}))
        parse'osmosis_replication_timestamp
         = P'.try
            (do
               v <- P'.getT "osmosis_replication_timestamp"
               Prelude'.return (\ o -> o{osmosis_replication_timestamp = v}))
        parse'osmosis_replication_sequence_number
         = P'.try
            (do
               v <- P'.getT "osmosis_replication_sequence_number"
               Prelude'.return (\ o -> o{osmosis_replication_sequence_number = v}))
        parse'osmosis_replication_base_url
         = P'.try
            (do
               v <- P'.getT "osmosis_replication_base_url"
               Prelude'.return (\ o -> o{osmosis_replication_base_url = v}))