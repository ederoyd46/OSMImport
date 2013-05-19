{-# LANGUAGE DeriveGeneric #-}

module OSMFormat where

  import Data.Int
  import Data.ProtocolBuffers
  import Data.TypeLevel (D1, D2, D3, D4, D5, D8, D9, D10, D11, D16, D17, D18, D19, D20)
  import Data.Text
  import Data.Word
  import Data.ByteString
  import GHC.Generics (Generic)

  data BlobHeader = BlobHeader
     { bh_type :: Required D1 (Value Text)
     , bh_indexdata :: Optional D2 (Value Text)
     , bh_datasize :: Required D3 (Value Int32)
     } deriving (Generic, Show)

  instance Encode BlobHeader
  instance Decode BlobHeader

  data Blob = Blob
     { b_raw :: Optional D1 (Value ByteString) -- Not sure if this works...
     , b_raw_size :: Optional D2 (Value Int32)
     , b_zlib_data :: Optional D3 (Value ByteString)
     } deriving (Generic, Show)

  instance Encode Blob
  instance Decode Blob


  data HeaderBlock = HeaderBlock
     { hb_bbox :: Optional D1 (Message HeaderBBox)
     , hb_required_features :: Repeated D4 (Value Text)
     , hb_optional_features :: Repeated D5 (Value Text)
     , hb_writingprogram :: Optional D16 (Value Text)
     , hb_source :: Optional D17 (Value Text)
     } deriving (Generic, Show)
  
  instance Encode HeaderBlock
  instance Decode HeaderBlock

  data HeaderBBox = HeaderBBox
     { hbbox_left :: Required D1 (Value (Signed Int64))
     , hbbox_right :: Required D2 (Value (Signed Int64))
     , hbbox_top :: Required D3 (Value (Signed Int64))
     , hbbox_bottom :: Required D4 (Value (Signed Int64))
     } deriving (Generic, Show)

  instance Encode HeaderBBox
  instance Decode HeaderBBox

  data PrimitiveBlock = PrimitiveBlock
    { pb_stringtable :: Required D1 (Message StringTable)
    , pb_primitivegroup :: Repeated D2 (Message PrimitiveGroup)
    -- Granularity, units of nanodegrees, used to store coordinates in this block
    , pb_granularity :: Optional D17 (Value Int32) -- [default=100];
     -- Offset value between the output coordinates coordinates and the granularity grid in unites of nanodegrees.
    , pb_lat_offset :: Optional D19 (Value Int64) -- [default=0];
    , pb_lon_offset :: Optional D20 (Value Int64) -- [default=0];
      -- Granularity of dates, normally represented in units of milliseconds since the 1970 epoch.
    , pb_date_granularity :: Optional D18 (Value Int32) -- [default=1000];
    } deriving (Generic, Show)

  instance Encode PrimitiveBlock
  instance Decode PrimitiveBlock

  data StringTable = StringTable
    { st_bytes :: Repeated D1 (Value ByteString)
    } deriving (Generic, Show)

  instance Encode StringTable
  instance Decode StringTable

-- // Group of OSMPrimitives. All primitives in a group must be the same type.
  data PrimitiveGroup = PrimitiveGroup
    { pg_nodes :: Repeated D1 (Message Node)
    , pg_dense :: Optional D2 (Message DenseNodes)
    , pg_ways :: Repeated D3 (Message Way)
    , pg_relations :: Repeated D4 (Message Relation)
    , pg_change_sets :: Repeated D5 (Message ChangeSet)
    } deriving (Generic, Show)
    
  instance Encode PrimitiveGroup
  instance Decode PrimitiveGroup
    
  -- /** Optional metadata that may be included into each primitive. Special dense format used in DenseNodes. */
  data DenseInfo = DenseInfo
    { dense_info_version :: Packed D1 (Value Int32)
    , dense_info_timestamp :: Packed D2 (Value (Signed Int64))
    , dense_info_changeset :: Packed D3 (Value (Signed Int64))
    , dense_info_uid :: Packed D4 (Value (Signed Int32))
    , dense_info_user_sid :: Packed D5 (Value (Signed Int32)) -- String IDs
    } deriving (Generic, Show)

  instance Encode DenseInfo
  instance Decode DenseInfo

  data DenseNodes = DenseNodes
    { dense_nodes_id :: Packed D1 (Value (Signed Int64))
    , dense_nodes_info :: Optional D5 (Message DenseInfo)
    , dense_nodes_lat :: Packed D8 (Value (Signed Int64))
    , dense_nodes_lon :: Packed D9 (Value (Signed Int64))
    , dense_nodes_keys_vals :: Packed D10 (Value Int32)
    } deriving (Generic, Show)
    
  instance Encode DenseNodes
  instance Decode DenseNodes

  data Way = Way
    { way_id :: Required D1 (Value Int64)
    -- , way_keys :: Packed D2 (Value Int32) -- [packed = true];
    -- , way_vals :: Packed D3 (Value Int32) -- [packed = true];
    , way_info :: Optional D4 (Message Info)
    , way_refs :: Packed D8 (Value (Signed Int64)) -- [packed = true];  // DELTA coded
    } deriving (Generic, Show)

  instance Encode Way
  instance Decode Way

  data Relation = Relation 
    { relation_id :: Required D1 (Value Int64)
    -- , relation_keys :: Packed D2 (Value Int32) -- [packed = true];
    -- , relation_vals :: Packed D3 (Value Int32) -- [packed = true];
    , relation_info :: Optional D4 (Message Info)
    , relation_roles_sid :: Packed D8 (Value Int32) -- [packed = true];
    , relation_memids :: Packed D9 (Value (Signed Int64)) -- [packed = true];
    } deriving (Generic, Show)
  -- //Need to add member types
  instance Encode Relation
  instance Decode Relation

  --  Optional metadata that may be included into each primitive.
  data Info = Info
    { info_version :: Optional D1 (Value Int32)
    , info_timestamp :: Optional D2 (Value Int32)
    , info_changeset :: Optional D3 (Value Int64)
    , info_uid :: Optional D4 (Value Int32)
    , info_user_sid :: Optional D5 (Value Int32)
    } deriving (Generic, Show)

  instance Encode Info
  instance Decode Info

-- Unused

  data Node = Node
    { node_id :: Required D1 (Value (Signed Int64))
    , node_keys :: Packed D2 (Value Int32) -- [packed = true]; // String IDs.
    , node_vals :: Packed D3 (Value Int32) -- [packed = true]; // String IDs.
    , node_info :: Optional D4 (Message Info)
    , node_lat :: Required D8 (Value Int64)
    , node_lon :: Required D9 (Value Int64)
    } deriving (Generic, Show)

  instance Encode Node
  instance Decode Node

-- // TODO: REMOVE THIS? NOT in osmosis schema.
  data ChangeSet = ChangeSet
    { change_set_id :: Required D1 (Value Int64)
    -- Parallel arrays.
    , change_set_keys :: Packed D2 (Value Int32) -- [packed = true]; // String IDs.
    , change_set_vals :: Packed D3 (Value Int32) -- [packed = true]; // String IDs.
    , change_set_info :: Optional D4 (Message Info)
    , change_set_created_at :: Required D8 (Value Int64)
    , change_set_closetime_delta :: Optional D9 (Value Int64)
    , change_set_open :: Required D10 (Value Int64)
    , change_set_bbox :: Optional D11 (Message HeaderBBox)
    } deriving (Generic, Show)

  instance Encode ChangeSet
  instance Decode ChangeSet


  