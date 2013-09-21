module Data.Node where
  import Data.Tag

  data ImportNode = ImportNode {  _id          :: Integer
                                , latitude     :: Float
                                , longitude    :: Float
                                , tags         :: [ImportTag]
                                , version      :: Maybe Integer
                                , timestamp    :: Maybe Integer
                                , changeset    :: Maybe Integer
                                , uid          :: Maybe Integer
                                , sid          :: Maybe String
                               }  deriving (Show)



