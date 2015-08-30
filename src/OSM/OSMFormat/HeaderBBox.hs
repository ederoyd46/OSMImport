{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module OSM.OSMFormat.HeaderBBox (HeaderBBox(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data HeaderBBox = HeaderBBox{left :: !(P'.Int64), right :: !(P'.Int64), top :: !(P'.Int64), bottom :: !(P'.Int64),
                             unknown'field :: !(P'.UnknownField)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage HeaderBBox where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable HeaderBBox where
  mergeAppend (HeaderBBox x'1 x'2 x'3 x'4 x'5) (HeaderBBox y'1 y'2 y'3 y'4 y'5)
   = HeaderBBox (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
 
instance P'.Default HeaderBBox where
  defaultValue = HeaderBBox P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire HeaderBBox where
  wireSize ft' self'@(HeaderBBox x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 18 x'1 + P'.wireSizeReq 1 18 x'2 + P'.wireSizeReq 1 18 x'3 + P'.wireSizeReq 1 18 x'4 +
             P'.wireSizeUnknownField x'5)
  wirePut ft' self'@(HeaderBBox x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 18 x'1
             P'.wirePutReq 16 18 x'2
             P'.wirePutReq 24 18 x'3
             P'.wirePutReq 32 18 x'4
             P'.wirePutUnknownField x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{left = new'Field}) (P'.wireGet 18)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{right = new'Field}) (P'.wireGet 18)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{top = new'Field}) (P'.wireGet 18)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{bottom = new'Field}) (P'.wireGet 18)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> HeaderBBox) HeaderBBox where
  getVal m' f' = f' m'
 
instance P'.GPB HeaderBBox
 
instance P'.ReflectDescriptor HeaderBBox where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24, 32]) (P'.fromDistinctAscList [8, 16, 24, 32])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Osmformat.HeaderBBox\", haskellPrefix = [], parentModule = [MName \"OSM\",MName \"OSMFormat\"], baseName = MName \"HeaderBBox\"}, descFilePath = [\"OSM\",\"OSMFormat\",\"HeaderBBox.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBBox.left\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBBox\"], baseName' = FName \"left\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBBox.right\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBBox\"], baseName' = FName \"right\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBBox.top\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBBox\"], baseName' = FName \"top\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Osmformat.HeaderBBox.bottom\", haskellPrefix' = [], parentModule' = [MName \"OSM\",MName \"OSMFormat\",MName \"HeaderBBox\"], baseName' = FName \"bottom\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"
 
instance P'.TextType HeaderBBox where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg HeaderBBox where
  textPut msg
   = do
       P'.tellT "left" (left msg)
       P'.tellT "right" (right msg)
       P'.tellT "top" (top msg)
       P'.tellT "bottom" (bottom msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'left, parse'right, parse'top, parse'bottom]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'left
         = P'.try
            (do
               v <- P'.getT "left"
               Prelude'.return (\ o -> o{left = v}))
        parse'right
         = P'.try
            (do
               v <- P'.getT "right"
               Prelude'.return (\ o -> o{right = v}))
        parse'top
         = P'.try
            (do
               v <- P'.getT "top"
               Prelude'.return (\ o -> o{top = v}))
        parse'bottom
         = P'.try
            (do
               v <- P'.getT "bottom"
               Prelude'.return (\ o -> o{bottom = v}))