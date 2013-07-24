{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module OSM.OSMFormat.DenseInfo (DenseInfo(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data DenseInfo = DenseInfo{version :: !(P'.Seq P'.Int32), timestamp :: !(P'.Seq P'.Int64), changeset :: !(P'.Seq P'.Int64),
                           uid :: !(P'.Seq P'.Int32), user_sid :: !(P'.Seq P'.Int32), unknown'field :: !P'.UnknownField}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage DenseInfo where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable DenseInfo where
  mergeAppend (DenseInfo x'1 x'2 x'3 x'4 x'5 x'6) (DenseInfo y'1 y'2 y'3 y'4 y'5 y'6)
   = DenseInfo (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default DenseInfo where
  defaultValue = DenseInfo P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire DenseInfo where
  wireSize ft' self'@(DenseInfo x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizePacked 1 5 x'1 + P'.wireSizePacked 1 18 x'2 + P'.wireSizePacked 1 18 x'3 + P'.wireSizePacked 1 17 x'4 +
             P'.wireSizePacked 1 17 x'5
             + P'.wireSizeUnknownField x'6)
  wirePut ft' self'@(DenseInfo x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutPacked 10 5 x'1
             P'.wirePutPacked 18 18 x'2
             P'.wirePutPacked 26 18 x'3
             P'.wirePutPacked 34 17 x'4
             P'.wirePutPacked 42 17 x'5
             P'.wirePutUnknownField x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{version = P'.append (version old'Self) new'Field}) (P'.wireGet 5)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{version = P'.mergeAppend (version old'Self) new'Field})
                    (P'.wireGetPacked 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = P'.append (timestamp old'Self) new'Field}) (P'.wireGet 18)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{timestamp = P'.mergeAppend (timestamp old'Self) new'Field})
                    (P'.wireGetPacked 18)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{changeset = P'.append (changeset old'Self) new'Field}) (P'.wireGet 18)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{changeset = P'.mergeAppend (changeset old'Self) new'Field})
                    (P'.wireGetPacked 18)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{uid = P'.append (uid old'Self) new'Field}) (P'.wireGet 17)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{uid = P'.mergeAppend (uid old'Self) new'Field}) (P'.wireGetPacked 17)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{user_sid = P'.append (user_sid old'Self) new'Field}) (P'.wireGet 17)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{user_sid = P'.mergeAppend (user_sid old'Self) new'Field})
                    (P'.wireGetPacked 17)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> DenseInfo) DenseInfo where
  getVal m' f' = f' m'
 
instance P'.GPB DenseInfo
 
instance P'.ReflectDescriptor DenseInfo where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 10, 16, 18, 24, 26, 32, 34, 40, 42])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.DenseInfo\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"DenseInfo\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"DenseInfo.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseInfo.version\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseInfo\"], baseName' = FName \"version\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Just (WireTag {getWireTag = 8},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseInfo.timestamp\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseInfo\"], baseName' = FName \"timestamp\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseInfo.changeset\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseInfo\"], baseName' = FName \"changeset\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Just (WireTag {getWireTag = 24},WireTag {getWireTag = 26}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseInfo.uid\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseInfo\"], baseName' = FName \"uid\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Just (WireTag {getWireTag = 32},WireTag {getWireTag = 34}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.DenseInfo.user_sid\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"DenseInfo\"], baseName' = FName \"user_sid\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Just (WireTag {getWireTag = 40},WireTag {getWireTag = 42}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"