{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}

module Dto where
  
  import Data.Int
  
  data ParseTag = ParseTag {  key :: String
                             ,value :: String
                           } deriving (Show)
  
  data ImportNode = ImportNode { id :: Integer
                                ,latitude :: Float
                                ,longitude :: Float
                                ,tags :: [ParseTag]
                               } deriving (Show)
