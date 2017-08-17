{-# LANGUAGE OverloadedStrings #-}

module Database where
  
  import qualified Data.Node as N
  import Data.Tag
  import qualified Data.Way as W
  import qualified Data.Relation as R
  import Database.MongoDB
  import qualified Data.Text as T
  import Control.Monad.IO.Class
 
  openConnection :: String -> IO Pipe
  openConnection dbconnection =
    connect $ readHostPort dbconnection

  runDBCommand :: MonadIO m => Pipe -> String -> Action m a -> m a
  runDBCommand pipe dbname command = do
    access pipe master (T.pack dbname) command

  closeConnection :: Pipe -> IO ()
  closeConnection pipe = 
    close pipe

  saveNodes :: Pipe -> String -> [N.ImportNode] -> IO ()
  saveNodes pipe dbname nodes = do
    runDBCommand pipe dbname $ insertMany_ "node" (parseNodes nodes)
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

  saveWays :: Pipe -> String -> [W.ImportWay] -> IO ()
  saveWays pipe dbname ways = do
    runDBCommand pipe dbname $ insertMany_ "way" (parseWays ways)
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
                                                 
  saveRelation :: Pipe -> String -> [R.ImportRelation] -> IO ()
  saveRelation pipe dbname nodes = do
    runDBCommand pipe dbname $ insertMany_ "relation" (parseRelation nodes)
    where
      parseRelation [] = []
      parseRelation (x:xs) = [ "_id" =: (R._id x)
                          , "tags" =: (parseTags (R.tags x))
                          , "version" =: (R.version x)
                          , "timestamp" =: (R.timestamp x)
                          , "changeset" =: (R.changeset x)
                          , "user" =: (R.user x)
                          , "members" =: (parseTags (R.members x))
                          ] : parseRelation xs
  
  parseTags :: [ImportTag] -> [Field]
  parseTags [] = []
  parseTags (x:xs) = ((T.pack $ Data.Tag.key x) =: (Data.Tag.value x)) : parseTags xs