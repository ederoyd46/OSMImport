module Data.Relation where
  import Data.Tag

  data ImportRelation = ImportRelation {  _id          :: Integer
                                        , tags         :: [ImportTag]
                                        , version      :: Maybe Integer
                                        , timestamp    :: Maybe Integer
                                        , changeset    :: Maybe Integer
                                        , user         :: Maybe String
                                        , members      :: [ImportTag]
                                       } deriving (Show)

