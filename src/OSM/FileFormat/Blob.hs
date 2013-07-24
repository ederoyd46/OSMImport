{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module OSM.FileFormat.Blob (Blob(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Blob = Blob{raw :: !(P'.Maybe P'.ByteString), raw_size :: !(P'.Maybe P'.Int32), zlib_data :: !(P'.Maybe P'.ByteString),
                 lzma_data :: !(P'.Maybe P'.ByteString), bzip2_data :: !(P'.Maybe P'.ByteString), unknown'field :: !P'.UnknownField}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage Blob where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable Blob where
  mergeAppend (Blob x'1 x'2 x'3 x'4 x'5 x'6) (Blob y'1 y'2 y'3 y'4 y'5 y'6)
   = Blob (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default Blob where
  defaultValue = Blob P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Blob where
  wireSize ft' self'@(Blob x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 12 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeOpt 1 12 x'3 + P'.wireSizeOpt 1 12 x'4 +
             P'.wireSizeOpt 1 12 x'5
             + P'.wireSizeUnknownField x'6)
  wirePut ft' self'@(Blob x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 12 x'1
             P'.wirePutOpt 16 5 x'2
             P'.wirePutOpt 26 12 x'3
             P'.wirePutOpt 34 12 x'4
             P'.wirePutOpt 42 12 x'5
             P'.wirePutUnknownField x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{raw = Prelude'.Just new'Field}) (P'.wireGet 12)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{raw_size = Prelude'.Just new'Field}) (P'.wireGet 5)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{zlib_data = Prelude'.Just new'Field}) (P'.wireGet 12)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{lzma_data = Prelude'.Just new'Field}) (P'.wireGet 12)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{bzip2_data = Prelude'.Just new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Blob) Blob where
  getVal m' f' = f' m'
 
instance P'.GPB Blob
 
instance P'.ReflectDescriptor Blob where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 16, 26, 34, 42])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Fileformat.Blob\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"FileFormat\"], baseName = MName \"Blob\"}, descFilePath = [\"OSM\",\"FileFormat\",\"Blob.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.Blob.raw\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"Blob\"], baseName' = FName \"raw\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.Blob.raw_size\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"Blob\"], baseName' = FName \"raw_size\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.Blob.zlib_data\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"Blob\"], baseName' = FName \"zlib_data\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.Blob.lzma_data\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"Blob\"], baseName' = FName \"lzma_data\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Fileformat.Blob.bzip2_data\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"FileFormat\",MName \"Blob\"], baseName' = FName \"bzip2_data\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"