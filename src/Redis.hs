{-# LANGUAGE OverloadedStrings #-}

module Redis where
  import Database.Redis
  import Common
  import Data.ByteString.Char8
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
  saveNodes _ _ [] = return()
  saveNodes hostname port (node:nodes) = do
    saveNode hostname port node 
    saveNodes hostname port nodes 
  
  saveNode :: String -> Int -> ImportNode -> IO ()
  saveNode hostname port node = do
    conn <- connectRedis hostname port
    runRedis conn $ do
      select redisNodes
      Right created <- hset (parse $ _id node) "latitude" (parse $ latitude node)
      return ()
    return ()
    where
      parse a = pack (show a)
        
  
  
  saveNodes' :: String -> Int -> [ImportNode] -> IO ()
  saveNodes' hostname port nodes = do
    conn <- connectRedis hostname port
    runRedis conn $ do
      select redisNodes
      forM_ nodes $ \i -> do
        created <- hset (parse $ _id i) "latitude" (parse $ latitude i)
        return()
      return()
    return()
    where 
      parse a = pack (show a)
      