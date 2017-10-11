{-# LANGUAGE OverloadedStrings #-}

module Database where
  import Types
  import Database.MongoDB hiding(timestamp)
  import qualified Data.Text as T
  import Control.Monad.IO.Class
  import Control.Concurrent(forkIO, ThreadId)
  -- import GHC.Conc.Sync.ThreadId
  

  openConnection :: String -> IO Pipe
  openConnection dbconnection =
    connect $ readHostPort dbconnection

  runDBCommand :: MonadIO m => Pipe -> String -> Action m a -> m a
  runDBCommand pipe dbname command = do
    access pipe master (T.pack dbname) command

  closeConnection :: Pipe -> IO ()
  closeConnection pipe = 
    close pipe

  saveNodes :: Pipe -> String -> [ImportNode] -> IO ()
  saveNodes pipe dbname nodes = do
    forkIO $ runDBCommand pipe dbname $ insertMany_ "node" (parseNodes nodes)
    return ()
    where
      parseNodes [] = []
      parseNodes (ImportNode nodeId latitude longitude tags version timestamp changeset uid sid :xs) 
                        = [ "_id" =: nodeId
                          , "latitude" =: latitude
                          , "longitude" =: longitude
                          , "tags" =: parseTags tags
                          , "version" =: version
                          , "timestamp" =: timestamp
                          , "changeset" =: changeset
                          , "uid" =: uid
                          , "user" =: sid
                        ] : parseNodes xs

  saveWays :: Pipe -> String -> [ImportWay] -> IO ()
  saveWays pipe dbname ways = do
    runDBCommand pipe dbname $ insertMany_ "way" (parseWays ways)
    where
      parseWays [] = []
      parseWays (ImportWay wayId tags version timestamp changeset uid user nodes :xs) 
                        = [ "_id" =: wayId
                          , "tags" =: parseTags tags
                          , "version" =: version
                          , "timestamp" =: timestamp
                          , "changeset" =: changeset
                          , "uid" =: uid
                          , "user" =: user
                          , "nodes" =: nodes
                        ] : parseWays xs
                                                 
  saveRelation :: Pipe -> String -> [ImportRelation] -> IO ()
  saveRelation pipe dbname relations = do
    runDBCommand pipe dbname $ insertMany_ "relation" (parseRelations relations)
    where
      parseRelations [] = []
      parseRelations (ImportRelation relationId tags version timestamp changeset user members :xs) 
                        = [ "_id" =: relationId
                          , "tags" =: parseTags tags
                          , "version" =: version
                          , "timestamp" =: timestamp
                          , "changeset" =: changeset
                          , "user" =: user
                          , "members" =: parseTags members
                        ] : parseRelations xs
  
  parseTags :: [ImportTag] -> [Field]
  parseTags [] = []
  parseTags (ImportTag k v:xs) = ((T.pack $ k) =: v) : parseTags xs