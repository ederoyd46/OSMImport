{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module OSM.OSMFormat.PrimitiveGroup (PrimitiveGroup(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.ChangeSet as OSM.OSMFormat (ChangeSet)
import qualified OSM.OSMFormat.DenseNodes as OSM.OSMFormat (DenseNodes)
import qualified OSM.OSMFormat.Node as OSM.OSMFormat (Node)
import qualified OSM.OSMFormat.Relation as OSM.OSMFormat (Relation)
import qualified OSM.OSMFormat.Way as OSM.OSMFormat (Way)

data PrimitiveGroup = PrimitiveGroup{nodes :: !(P'.Seq OSM.OSMFormat.Node), dense :: !(P'.Maybe OSM.OSMFormat.DenseNodes),
                                     ways :: !(P'.Seq OSM.OSMFormat.Way), relations :: !(P'.Seq OSM.OSMFormat.Relation),
                                     changesets :: !(P'.Seq OSM.OSMFormat.ChangeSet), unknown'field :: !(P'.UnknownField)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

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
  wirePutWithSize ft' self'@(PrimitiveGroup x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 10 11 x'1, P'.wirePutOptWithSize 18 11 x'2, P'.wirePutRepWithSize 26 11 x'3,
             P'.wirePutRepWithSize 34 11 x'4, P'.wirePutRepWithSize 42 11 x'5, P'.wirePutUnknownFieldWithSize x'6]
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
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.PrimitiveGroup\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"PrimitiveGroup\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"PrimitiveGroup.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.nodes\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"nodes\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Node\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Node\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.dense\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"dense\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.DenseNodes\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"DenseNodes\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.ways\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"ways\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Way\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Way\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.relations\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"relations\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Relation\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Relation\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.PrimitiveGroup.changesets\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"PrimitiveGroup\"], baseName' = FName \"changesets\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.ChangeSet\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"ChangeSet\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType PrimitiveGroup where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg PrimitiveGroup where
  textPut msg
   = do
       P'.tellT "nodes" (nodes msg)
       P'.tellT "dense" (dense msg)
       P'.tellT "ways" (ways msg)
       P'.tellT "relations" (relations msg)
       P'.tellT "changesets" (changesets msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'nodes, parse'dense, parse'ways, parse'relations, parse'changesets]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'nodes
         = P'.try
            (do
               v <- P'.getT "nodes"
               Prelude'.return (\ o -> o{nodes = P'.append (nodes o) v}))
        parse'dense
         = P'.try
            (do
               v <- P'.getT "dense"
               Prelude'.return (\ o -> o{dense = v}))
        parse'ways
         = P'.try
            (do
               v <- P'.getT "ways"
               Prelude'.return (\ o -> o{ways = P'.append (ways o) v}))
        parse'relations
         = P'.try
            (do
               v <- P'.getT "relations"
               Prelude'.return (\ o -> o{relations = P'.append (relations o) v}))
        parse'changesets
         = P'.try
            (do
               v <- P'.getT "changesets"
               Prelude'.return (\ o -> o{changesets = P'.append (changesets o) v}))