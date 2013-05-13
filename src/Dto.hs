{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}

module Dto where
  
  import Data.Int
  
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
