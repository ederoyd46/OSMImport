{-# LANGUAGE OverloadedStrings #-}

module Redis where
  import Database.Redis
  import Common
  import Data.ByteString.Char8
  import Data.Maybe(fromMaybe)
  import Control.Monad (forM_)
  
  redisNodes :: Integer
  redisNodes = 1
  
  connectRedis :: String -> Int -> IO Connection
  connectRedis hostname port = do 
    conn <- connect info 
    return conn
    where
      info = ConnInfo { connectHost           = hostname
                      , connectPort           = PortNumber (fromIntegral port)
                      , connectAuth           = Nothing
                      , connectMaxConnections = 50
                      , connectMaxIdleTime    = 30
                      }
  
  saveNodes :: String -> Int -> [ImportNode] -> IO ()
  saveNodes hostname port nodes = do
    conn <- connectRedis hostname port
    runRedis conn $ do
      select redisNodes
      forM_ nodes $ \i -> do
        created <- hmset (parse $ _id i) (buildHash i)
        return()
      return()
    return()
    where 
      parse a = pack (show a)
      
      buildHash :: ImportNode -> [(ByteString, ByteString)]
      buildHash node = [ ("_id", (parse $ _id node))
                        ,("latitude", (parse $ latitude node))
                        ,("longitude", (parse $ longitude node))
                        ,("version", parse $ fromMaybe 0 (version node))
                        ,("timestamp", parse $ fromMaybe 0 (timestamp node))
                        ,("changeset", parse $ fromMaybe 0 (changeset node))
                        ,("uid", parse $ fromMaybe 0 (uid node))
                        ,("user", pack $ fromMaybe "" (sid node))
                        ] ++ parseTags (tags node) []
      
      parseTags [] [] = []
      parseTags [] y = Prelude.reverse y
      parseTags (x:xs) y =
        parseTags xs (((pack $ key x), (pack $ Common.value x)) : y)
      
    