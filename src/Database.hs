{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where
  
  import Dto
  import Database.MongoDB
  import qualified Data.Text as T

  runDBCommand :: String -> String -> Action IO a -> IO ()  
  runDBCommand dbconnection dbname command = do
    pipe <- runIOE $ connect (readHostPort dbconnection)
    e <- access pipe master (T.pack dbname) command
    close pipe

  saveNodes :: String -> String -> [ImportNode] -> IO ()
  saveNodes _ _ [] = return ()
  saveNodes dbconnection dbname (x:xs) = do 
    saveNode x
    saveNodes dbconnection dbname xs
    where
      saveNode :: ImportNode -> IO ()
      saveNode node = do
        let insertNode = insert "node" [ "_id" =: (Dto.id node)
                                         , "latitude" =: (latitude node)
                                         , "longitude" =: (longitude node)
                                         , "tags" =: (parseTags (tags node) [])
                                         ]
        runDBCommand dbconnection dbname insertNode
      
      parseTags [] [] = []
      parseTags [] y = y
      parseTags (x:xs) y =
        parseTags xs (y ++ [(T.pack $ key x) =: (Dto.value x)])
