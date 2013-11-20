{-# LANGUAGE OverloadedStrings #-}

module Database where
  
  import qualified Data.Node as N
  import Data.Tag
  import qualified Data.Way as W
  import Database.MongoDB
  import qualified Data.Text as T
 
  runDBCommand :: String -> String -> Action IO a -> IO ()  
  runDBCommand dbconnection dbname command = do
    pipe <- runIOE $ connect (readHostPort dbconnection)
    _ <- access pipe master (T.pack dbname) command
    close pipe

  saveNodes :: String -> String -> [N.ImportNode] -> IO ()
  saveNodes dbconnection dbname nodes = do
    let insertNodes = insertMany "node" (parseNodes nodes)
    runDBCommand dbconnection dbname insertNodes

    where
      parseNodes [] = []
      parseNodes (x:xs) = [ "_id" =: (N._id x)
                          , "latitude" =: (N.latitude x)
                          , "longitude" =: (N.longitude x)
                          , "tags" =: (parseTags (N.tags x))
                          , "version" =: (N.version x)
                          , "timestamp" =: (N.timestamp x)
                          , "changeset" =: (N.changeset x)
                          , "uid" =: (N.uid x)
                          , "user" =: (N.sid x)
                          ] : parseNodes xs


  saveWays :: String -> String -> [W.ImportWay] -> IO ()
  saveWays dbconnection dbname nodes = do
    let insertWays = insertMany "way" (parseWays nodes)
    runDBCommand dbconnection dbname insertWays
    where
      parseWays [] = []
      parseWays (x:xs) = [ "_id" =: (W._id x)
                          , "tags" =: (parseTags (W.tags x))
                          , "version" =: (W.version x)
                          , "timestamp" =: (W.timestamp x)
                          , "changeset" =: (W.changeset x)
                          , "uid" =: (W.uid x)
                          , "user" =: (W.user x)
                          , "nodes" =: (W.nodes x)
                          ] : parseWays xs

  parseTags [] = []
  parseTags (x:xs) = ((T.pack $ Data.Tag.key x) =: (Data.Tag.value x)) : parseTags xs

