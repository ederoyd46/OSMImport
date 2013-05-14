module Common where
  
  data ImportTag = ImportTag {  key     :: String
                               ,value   :: String
                           } deriving (Show)
  
  data ImportNode = ImportNode { _id          :: Integer
                                ,latitude     :: Float
                                ,longitude    :: Float
                                ,tags         :: [ImportTag]
                               } |
                    ImportAugmentedNode {  _id          :: Integer
                                          ,latitude     :: Float
                                          ,longitude    :: Float
                                          ,tags         :: [ImportTag]
                                          ,changeset    :: String
                                          ,uid          :: String
                                          ,version      :: Integer
                                          ,user         :: String
                                          ,time         :: Integer
                                        } deriving (Show)




  deltaDecode :: Num a => [a] -> a -> [a] -> [a]
  deltaDecode [] _ [] = []
  deltaDecode [] _ rest = rest
  deltaDecode (x:xs) offset rest = do
    let lastId = offset + x
    deltaDecode xs lastId (rest ++ [lastId])

  nano :: Float
  nano = 1000000000
