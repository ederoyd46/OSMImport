{-# LANGUAGE OverloadedStrings #-}

import Importer
import OSMFormat
import Common
import qualified Database as MDB
import qualified Redis as R
import qualified Data.Serialize as S
import Data.ProtocolBuffers
import Control.Concurrent (forkIO, MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad(when, forever)
import GHC.Generics (Generic)
import Data.Maybe(fromJust, isJust, isNothing)
import Data.Binary.Get
import Codec.Compression.Zlib
import System.Exit
import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BCE
import Data.List.Split


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

