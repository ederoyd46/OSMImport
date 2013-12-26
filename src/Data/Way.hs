module Data.Way where
  import Data.Tag

  data ImportWay = ImportWay {  _id          :: Integer
                              , tags         :: [ImportTag]
                              , version      :: Integer
                              , timestamp    :: Integer
                              , changeset    :: Integer
                              , uid			     :: Integer
                              , user         :: String
                              , nodes        :: [Integer]
                            } deriving (Show)

