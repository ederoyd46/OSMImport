module Common where
 
  deltaDecode :: Num a => [a] -> a -> [a] -> [a]
  deltaDecode [] _ [] = []
  deltaDecode [] _ rest = reverse rest
  deltaDecode (x:xs) offset rest = do
    let lastId = offset + x
    deltaDecode xs lastId (lastId : rest)

  nano :: Float
  nano = 1000000000

  calculateDegrees :: [Integer] -> [Float] -> Integer -> [Float]
  calculateDegrees [] [] _ = []
  calculateDegrees [] y _ = reverse y
  calculateDegrees (x:xs) y gran = do
    let newcoordinate = fromIntegral (x * gran) / nano
    calculateDegrees xs (newcoordinate : y) gran

    
  