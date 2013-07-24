{-# LANGUAGE OverloadedStrings #-}

import Importer
import Control.Monad(when)
import System.Environment


main :: IO ()
main =  do 
  args <- getArgs
  when (length args < 4) $ showUsage
  
  let dbtype = args !! 0
  let dbconnection = args !! 1
  let dbname = args !! 2
  let filename = args !! 3
  startImport dbtype dbconnection dbname filename
  return ()

