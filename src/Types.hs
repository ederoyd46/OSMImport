module Types where
    type Id                 = Integer
    type Latitude           = Float
    type Longitude          = Float
    type Tags               = [ImportTag]
    type Version            = Integer
    type Timestamp          = Integer
    type Changeset          = Integer
    type UID                = Integer
    type SID                = String
    type User               = String
    type Members            = Tags

    type Key                = String
    type Value              = String

    type Nodes              = [Integer]

    data ImportTag          = ImportTag Key Value deriving (Show)

    data ImportNode         = ImportNode Id Latitude Longitude Tags Version Timestamp Changeset UID SID deriving Show
    data ImportWay          = ImportWay Id Tags Version Timestamp Changeset UID User Nodes deriving Show
    data ImportRelation     = ImportRelation Id Tags Version Timestamp Changeset User Members deriving Show
    