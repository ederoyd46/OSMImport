{-# LANGUAGE OverloadedStrings #-}

import Importer
import Control.Monad(when)
import System.Environment

main :: IO ()
main =  do 
  args <- getArgs
  when (length args < 3) $ showUsage
  
  let dbconnection = args !! 0
  let dbname = args !! 1
  let filename = args !! 2
  startImport dbconnection dbname filename
  return ()
