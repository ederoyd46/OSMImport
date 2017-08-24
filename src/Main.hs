{-# LANGUAGE OverloadedStrings #-}

import Importer
import Control.Monad(when)
import System.Environment
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Database (openConnection,closeConnection,saveNodes,saveWays,saveRelation)

main :: IO ()
main =  do 
  args <- getArgs
  when (length args < 3) $ showUsage
  
  let dbconnection = args !! 0
  let dbname = args !! 1
  let filename = args !! 2
  startMongoImport dbconnection dbname filename
  return ()

startMongoImport :: String -> String -> String -> IO ()
startMongoImport dbconnection dbname filename = do
  pipe <- openConnection dbconnection
  let dbNodecommand = saveNodes pipe dbname
  let dbWaycommand = saveWays pipe dbname
  let dbRelationcommand = saveRelation pipe dbname
  performImport filename dbNodecommand dbWaycommand dbRelationcommand
  closeConnection pipe  

showUsage :: IO ()
showUsage = do
      hPutStrLn stderr "usage: dbconnection dbname filename"
      hPutStrLn stderr "example: OSMImport '127.0.0.1:27017' 'geo_data' './download/england-latest.osm.pbf'"
      exitFailure
  