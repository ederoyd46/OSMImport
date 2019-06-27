{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module OSM.OSMFormat.Relation (Relation(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified OSM.OSMFormat.Info as OSM.OSMFormat (Info)
import qualified OSM.OSMFormat.Relation.MemberType as OSM.OSMFormat.Relation (MemberType)

data Relation = Relation{id :: !(P'.Int64), keys :: !(P'.Seq P'.Word32), vals :: !(P'.Seq P'.Word32),
                         info :: !(P'.Maybe OSM.OSMFormat.Info), roles_sid :: !(P'.Seq P'.Int32), memids :: !(P'.Seq P'.Int64),
                         types :: !(P'.Seq OSM.OSMFormat.Relation.MemberType), unknown'field :: !(P'.UnknownField)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage Relation where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable Relation where
  mergeAppend (Relation x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (Relation y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
   = Relation (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)

instance P'.Default Relation where
  defaultValue
   = Relation P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue

instance P'.Wire Relation where
  wireSize ft' self'@(Relation x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 3 x'1 + P'.wireSizePacked 1 13 x'2 + P'.wireSizePacked 1 13 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizePacked 1 5 x'5
             + P'.wireSizePacked 1 18 x'6
             + P'.wireSizePacked 1 14 x'7
             + P'.wireSizeUnknownField x'8)
  wirePutWithSize ft' self'@(Relation x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutReqWithSize 8 3 x'1, P'.wirePutPackedWithSize 18 13 x'2, P'.wirePutPackedWithSize 26 13 x'3,
             P'.wirePutOptWithSize 34 11 x'4, P'.wirePutPackedWithSize 66 5 x'5, P'.wirePutPackedWithSize 74 18 x'6,
             P'.wirePutPackedWithSize 82 14 x'7, P'.wirePutUnknownFieldWithSize x'8]
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{id = new'Field}) (P'.wireGet 3)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{keys = P'.append (keys old'Self) new'Field}) (P'.wireGet 13)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{keys = P'.mergeAppend (keys old'Self) new'Field}) (P'.wireGetPacked 13)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{vals = P'.append (vals old'Self) new'Field}) (P'.wireGet 13)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{vals = P'.mergeAppend (vals old'Self) new'Field}) (P'.wireGetPacked 13)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{info = P'.mergeAppend (info old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{roles_sid = P'.append (roles_sid old'Self) new'Field}) (P'.wireGet 5)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{roles_sid = P'.mergeAppend (roles_sid old'Self) new'Field})
                    (P'.wireGetPacked 5)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{memids = P'.append (memids old'Self) new'Field}) (P'.wireGet 18)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{memids = P'.mergeAppend (memids old'Self) new'Field})
                    (P'.wireGetPacked 18)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{types = P'.append (types old'Self) new'Field}) (P'.wireGet 14)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{types = P'.mergeAppend (types old'Self) new'Field}) (P'.wireGetPacked 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Relation) Relation where
  getVal m' f' = f' m'

instance P'.GPB Relation

instance P'.ReflectDescriptor Relation where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8, 16, 18, 24, 26, 34, 64, 66, 72, 74, 80, 82])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.Relation\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Relation\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"Relation.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.id\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"id\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.keys\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"keys\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.vals\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"vals\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Just (WireTag {getWireTag = 24},WireTag {getWireTag = 26}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.info\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"info\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Info\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"Info\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.roles_sid\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"roles_sid\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Just (WireTag {getWireTag = 64},WireTag {getWireTag = 66}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.memids\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"memids\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Just (WireTag {getWireTag = 72},WireTag {getWireTag = 74}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.Relation.types\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName' = FName \"types\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Just (WireTag {getWireTag = 80},WireTag {getWireTag = 82}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Osmformat.Relation.MemberType\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\",MName \"Relation\"], baseName = MName \"MemberType\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Relation where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Relation where
  textPut msg
   = do
       P'.tellT "id" (id msg)
       P'.tellT "keys" (keys msg)
       P'.tellT "vals" (vals msg)
       P'.tellT "info" (info msg)
       P'.tellT "roles_sid" (roles_sid msg)
       P'.tellT "memids" (memids msg)
       P'.tellT "types" (types msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'id, parse'keys, parse'vals, parse'info, parse'roles_sid, parse'memids, parse'types])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'id
         = P'.try
            (do
               v <- P'.getT "id"
               Prelude'.return (\ o -> o{id = v}))
        parse'keys
         = P'.try
            (do
               v <- P'.getT "keys"
               Prelude'.return (\ o -> o{keys = P'.append (keys o) v}))
        parse'vals
         = P'.try
            (do
               v <- P'.getT "vals"
               Prelude'.return (\ o -> o{vals = P'.append (vals o) v}))
        parse'info
         = P'.try
            (do
               v <- P'.getT "info"
               Prelude'.return (\ o -> o{info = v}))
        parse'roles_sid
         = P'.try
            (do
               v <- P'.getT "roles_sid"
               Prelude'.return (\ o -> o{roles_sid = P'.append (roles_sid o) v}))
        parse'memids
         = P'.try
            (do
               v <- P'.getT "memids"
               Prelude'.return (\ o -> o{memids = P'.append (memids o) v}))
        parse'types
         = P'.try
            (do
               v <- P'.getT "types"
               Prelude'.return (\ o -> o{types = P'.append (types o) v}))