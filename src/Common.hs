module Common where
  import Data.Int
  
  data ImportTag = ImportTag {  key     :: String
                               ,value   :: String
                           } deriving (Show)
  
  data ImportNode = ImportNode { _id          :: Integer
                                ,latitude     :: Float
                                ,longitude    :: Float
                                ,tags         :: [ImportTag]
                               } deriving (Show)


                    -- ImportAugmentedNode {  _id          :: Integer
                    --                       ,latitude     :: Float
                    --                       ,longitude    :: Float
                    --                       ,tags         :: [ImportTag]
                    --                       ,changeset    :: String
                    --                       ,uid          :: String
                    --                       ,version      :: Integer
                    --                       ,user         :: String
                    --                       ,time         :: Integer
                                        




  deltaDecode :: Num a => [a] -> a -> [a] -> [a]
  deltaDecode [] _ [] = []
  deltaDecode [] _ rest = reverse rest
  deltaDecode (x:xs) offset rest = do
    let lastId = offset + x
    deltaDecode xs lastId (lastId : rest)

  
  -- deltaDecode2 offset ids y
  --   | (length ids) > offset = deltaDecode2 (offset+1) ids (y ++ [(foldl (+) 0 $ take offset ids)])
  --   | otherwise = y
  -- 


  nano :: Float
  nano = 1000000000
