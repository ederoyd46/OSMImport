{-# LANGUAGE OverloadedStrings #-}

module Database where
  
  import qualified Data.Node as N
  import Data.Tag
  import qualified Data.Way as W
  import qualified Data.Relation as R
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
  saveWays dbconnection dbname ways = do
    let insertWays = insertMany "way" (parseWays ways)
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



  saveRelation :: String -> String -> [R.ImportRelation] -> IO ()
  saveRelation dbconnection dbname nodes = do
    let insertRelation = insertMany "relation" (parseRelation nodes)
    runDBCommand dbconnection dbname insertRelation
    where
      parseRelation [] = []
      parseRelation (x:xs) = [ "_id" =: (R._id x)
                          , "tags" =: (parseTags (R.tags x))
                          , "version" =: (R.version x)
                          , "timestamp" =: (R.timestamp x)
                          , "changeset" =: (R.changeset x)
                          , "user" =: (R.user x)
                          , "memids" =: (R.memids x)
                          ] : parseRelation xs

  parseTags [] = []
  parseTags (x:xs) = ((T.pack $ Data.Tag.key x) =: (Data.Tag.value x)) : parseTags xs

