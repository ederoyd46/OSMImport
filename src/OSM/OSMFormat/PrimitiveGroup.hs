{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module OSM.OSMFormat.PrimitiveGroup (PrimitiveGroup(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.ChangeSet as OSM.OSMFormat (ChangeSet)
import qualified OSM.OSMFormat.DenseNodes as OSM.OSMFormat (DenseNodes)
import qualified OSM.OSMFormat.Node as OSM.OSMFormat (Node)
import qualified OSM.OSMFormat.Relation as OSM.OSMFormat (Relation)
import qualified OSM.OSMFormat.Way as OSM.OSMFormat (Way)
 
data PrimitiveGroup = PrimitiveGroup{nodes :: !(P'.Seq OSM.OSMFormat.Node), dense :: !(P'.Maybe OSM.OSMFormat.DenseNodes),
                                     ways :: !(P'.Seq OSM.OSMFormat.Way), relations :: !(P'.Seq OSM.OSMFormat.Relation),
                                     changesets :: !(P'.Seq OSM.OSMFormat.ChangeSet), unknown'field :: !P'.UnknownField}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage PrimitiveGroup where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable PrimitiveGroup where
  mergeAppend (PrimitiveGroup x'1 x'2 x'3 x'4 x'5 x'6) (PrimitiveGroup y'1 y'2 y'3 y'4 y'5 y'6)
   = PrimitiveGroup (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default PrimitiveGroup where
  defaultValue = PrimitiveGroup P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire PrimitiveGroup where
  wireSize ft' self'@(PrimitiveGroup x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4 +
             P'.wireSizeRep 1 11 x'5
             + P'.wireSizeUnknownField x'6)
  wirePut ft' self'@(PrimitiveGroup x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
             P'.wirePutOpt 18 11 x'2
             P'.wirePutRep 26 11 x'3
             P'.wirePutRep 34 11 x'4
             P'.wirePutRep 42 11 x'5
             P'.wirePutUnknownField x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{nodes = P'.append (nodes old'Self) new'Field}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{dense = P'.mergeAppend (dense old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{ways = P'.append (ways old'Self) new'Field}) (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{relations = P'.append (relations old'Self) new'Field}) (P'.wireGet 11)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{changesets = P'.append (changesets old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> PrimitiveGroup) PrimitiveGroup where
  getVal m' f' = f' m'
 
instance P'.GPB PrimitiveGroup
 
instance P'.ReflectDescriptor PrimitiveGroup where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.PrimitiveGroup\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"PrimitiveGroup\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"PrimitiveGroup.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.nodes\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"nodes\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Node\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Node\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.dense\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"dense\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.DenseNodes\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"DenseNodes\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.ways\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"ways\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Way\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Way\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.relations\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"relations\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Relation\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Relation\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.changesets\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"changesets\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.ChangeSet\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"ChangeSet\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"