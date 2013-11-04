{-# LANGUAGE OverloadedStrings #-}

module Database where
  
  import Data.Node
  import Data.Tag
  import Database.MongoDB
  import qualified Data.Text as T
 
  runDBCommand :: String -> String -> Action IO a -> IO ()  
  runDBCommand dbconnection dbname command = do
    pipe <- runIOE $ connect (readHostPort dbconnection)
    _ <- access pipe master (T.pack dbname) command
    close pipe

  saveNodes :: String -> String -> [ImportNode] -> IO ()
  saveNodes dbconnection dbname nodes = do
    let insertNodes = insertMany "node" (parseNodes nodes [])
    runDBCommand dbconnection dbname insertNodes

    where
      parseNodes [] [] = []
      parseNodes [] y = reverse y
      parseNodes (x:xs) y = do
        let buildDoc = [ "_id" =: (_id x)
                       , "latitude" =: (latitude x)
                       , "longitude" =: (longitude x)
                       , "tags" =: (parseTags (tags x) [])
                       , "version" =: (version x)
                       , "timestamp" =: (Data.Node.timestamp x)
                       , "changeset" =: (changeset x)
                       , "uid" =: (uid x)
                       , "user" =: (sid x)
                       ]
        parseNodes xs (buildDoc : y)


  parseTags [] [] = []
  parseTags [] y = reverse y
  parseTags (x:xs) y =
    parseTags xs (((T.pack $ Data.Tag.key x) =: (Data.Tag.value x)) : y)

