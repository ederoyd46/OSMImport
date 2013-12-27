module Data.Relation where
  import Data.Tag

  data ImportRelation = ImportRelation {  _id          :: Integer
                                        , tags         :: [ImportTag]
                                        , version      :: Integer
                                        , timestamp    :: Integer
                                        , changeset    :: Integer
                                        , user         :: String
                                        , members      :: [ImportTag]
                                       } deriving (Show)

