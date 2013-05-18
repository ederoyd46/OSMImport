module Common where
  import Data.Int
  
  data ImportTag = ImportTag {  key     :: String
                               ,value   :: String
                           } deriving (Show)
  
  data ImportNode = ImportNode { _id          :: Integer
                                ,latitude     :: Float
                                ,longitude    :: Float
                                ,tags         :: [ImportTag]
                                ,version      :: Maybe Integer
                                ,timestamp    :: Maybe Integer
                                ,changeset    :: Maybe Integer
                                ,uid          :: Maybe Integer
                                ,sid          :: Maybe String
                               } deriving (Show)

  deltaDecode :: Num a => [a] -> a -> [a] -> [a]
  deltaDecode [] _ [] = []
  deltaDecode [] _ rest = reverse rest
  deltaDecode (x:xs) offset rest = do
    let lastId = offset + x
    deltaDecode xs lastId (lastId : rest)

  nano :: Float
  nano = 1000000000
