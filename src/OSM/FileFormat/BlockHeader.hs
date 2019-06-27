{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module OSM.FileFormat.BlockHeader (BlockHeader(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data BlockHeader = BlockHeader{type' :: !(P'.Utf8), indexdata :: !(P'.Maybe P'.ByteString), datasize :: !(P'.Int32),
                               unknown'field :: !(P'.UnknownField)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage BlockHeader where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable BlockHeader where
  mergeAppend (BlockHeader x'1 x'2 x'3 x'4) (BlockHeader y'1 y'2 y'3 y'4)
   = BlockHeader (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default BlockHeader where
  defaultValue = BlockHeader P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire BlockHeader where
  wireSize ft' self'@(BlockHeader x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeReq 1 5 x'3 + P'.wireSizeUnknownField x'4)
  wirePutWithSize ft' self'@(BlockHeader x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutReqWithSize 10 9 x'1, P'.wirePutOptWithSize 18 12 x'2, P'.wirePutReqWithSize 24 5 x'3,
             P'.wirePutUnknownFieldWithSize x'4]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{indexdata = Prelude'.Just new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{datasize = new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> BlockHeader) BlockHeader where
  getVal m' f' = f' m'

instance P'.GPB BlockHeader

instance P'.ReflectDescriptor BlockHeader where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 24]) (P'.fromDistinctAscList [10, 18, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Fileformat.BlockHeader\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"FileFormat\"], baseName = MName \"BlockHeader\"}, descFilePath = [\"OSM\",\"FileFormat\",\"BlockHeader.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.BlockHeader.type\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"BlockHeader\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.BlockHeader.indexdata\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"BlockHeader\"], baseName' = FName \"indexdata\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.BlockHeader.datasize\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"BlockHeader\"], baseName' = FName \"datasize\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType BlockHeader where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg BlockHeader where
  textPut msg
   = do
       P'.tellT "type" (type' msg)
       P'.tellT "indexdata" (indexdata msg)
       P'.tellT "datasize" (datasize msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'type', parse'indexdata, parse'datasize]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'indexdata
         = P'.try
            (do
               v <- P'.getT "indexdata"
               Prelude'.return (\ o -> o{indexdata = v}))
        parse'datasize
         = P'.try
            (do
               v <- P'.getT "datasize"
               Prelude'.return (\ o -> o{datasize = v}))