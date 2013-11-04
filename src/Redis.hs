{-# LANGUAGE OverloadedStrings #-}

module Redis where
  import Database.Redis
  import Data.Node
  import Data.Tag
  import Data.ByteString.Char8
  import Data.Maybe(fromMaybe)
  import Control.Monad (forM_)
  
  connectRedis :: String -> Int -> IO Connection
  connectRedis hostname port = do 
    conn <- connect info' 
    return conn
    where
      info' = ConnInfo {  connectHost           = hostname
                        , connectPort           = PortNumber (fromIntegral port)
                        , connectAuth           = Nothing
                        , connectMaxConnections = 50
                        , connectMaxIdleTime    = 30
                        }
  
  saveNodes :: String -> Int -> Int -> [ImportNode] -> IO ()
  saveNodes hostname port dbname nodes = do
    conn <- connectRedis hostname port
    runRedis conn $ do
      select $ fromIntegral dbname
      forM_ nodes $ \i -> do
        _ <- hmset (parse $ _id i) (buildHash i)
        return ()
    where 
      parse a = pack (show a)
      
      buildHash :: ImportNode -> [(ByteString, ByteString)]
      buildHash node = [ ("_id", (parse $ _id node))
                        ,("latitude", (parse $ latitude node))
                        ,("longitude", (parse $ longitude node))
                        ,("version", parse $ version node)
                        ,("timestamp", parse $ timestamp node)
                        ,("changeset", parse $ changeset node)
                        ,("uid", parse $ uid node)
                        ,("user", pack $ sid node)
                        ] ++ parseTags (tags node) []
      
      parseTags [] [] = []
      parseTags [] y = Prelude.reverse y
      parseTags (x:xs) y =
        parseTags xs (((pack $ Data.Tag.key x), (pack $ Data.Tag.value x)) : y)
      
    
