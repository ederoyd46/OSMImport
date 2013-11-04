module Data.Node where
  import Data.Tag

  data ImportNode = ImportNodeFull {  _id          :: Integer
                                    , latitude     :: Float
                                    , longitude    :: Float
                                    , tags         :: [ImportTag]
                                    , version      :: Integer
                                    , timestamp    :: Integer
                                    , changeset    :: Integer
                                    , uid          :: Integer
                                    , sid          :: String
                                   } |
                    ImportNodeSmall { _id          :: Integer
                                    , latitude     :: Float
                                    , longitude    :: Float
                                    , tags         :: [ImportTag]
                                    } deriving (Show)



