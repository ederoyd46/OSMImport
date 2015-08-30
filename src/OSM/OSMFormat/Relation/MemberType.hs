{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module OSM.OSMFormat.Relation.MemberType (MemberType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data MemberType = NODE
                | WAY
                | RELATION
                deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MemberType
 
instance Prelude'.Bounded MemberType where
  minBound = NODE
  maxBound = RELATION
 
instance P'.Default MemberType where
  defaultValue = NODE
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe MemberType
toMaybe'Enum 0 = Prelude'.Just NODE
toMaybe'Enum 1 = Prelude'.Just WAY
toMaybe'Enum 2 = Prelude'.Just RELATION
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum MemberType where
  fromEnum NODE = 0
  fromEnum WAY = 1
  fromEnum RELATION = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type OSM.OSMFormat.Relation.MemberType") .
      toMaybe'Enum
  succ NODE = WAY
  succ WAY = RELATION
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type OSM.OSMFormat.Relation.MemberType"
  pred WAY = NODE
  pred RELATION = WAY
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type OSM.OSMFormat.Relation.MemberType"
 
instance P'.Wire MemberType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB MemberType
 
instance P'.MessageAPI msg' (msg' -> MemberType) MemberType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum MemberType where
  reflectEnum = [(0, "NODE", NODE), (1, "WAY", WAY), (2, "RELATION", RELATION)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Osmformat.Relation.MemberType") [] ["OSM", "OSMFormat", "Relation"] "MemberType")
      ["OSM", "OSMFormat", "Relation", "MemberType.hs"]
      [(0, "NODE"), (1, "WAY"), (2, "RELATION")]
 
instance P'.TextType MemberType where
  tellT = P'.tellShow
  getT = P'.getRead