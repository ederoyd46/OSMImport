module Data.Way where
  import Data.Tag

  data ImportWay = ImportWay {  _id          :: Integer
                              , tags         :: [ImportTag]
                              , version      :: Maybe Integer
                              , timestamp    :: Maybe Integer
                              , changeset    :: Maybe Integer
                              , user         :: Maybe String
                              , nodes        :: [Integer]
                            } deriving (Show)

