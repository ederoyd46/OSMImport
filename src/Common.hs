module Common where

  nano :: Float
  nano = 1000000000

  calculateDegrees :: [Integer] -> Integer -> [Float]
  calculateDegrees [] _ = []
  calculateDegrees (x:xs) gran =
    fromIntegral (x * gran) / nano : calculateDegrees xs gran


  deltaDecode :: Num a => [a] -> [a]
  deltaDecode x = deltaDecode' x 0

  deltaDecode' :: Num a => [a] -> a -> [a]
  deltaDecode' [] _ = []
  deltaDecode' (x:xs) offset = (offset + x) : deltaDecode' xs (offset + x)