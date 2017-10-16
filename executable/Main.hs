{-# LANGUAGE OverloadedStrings #-}

module Silvi
 ( genLog
 ) where

import Control.Applicative
import Data.Attoparsec
import Data.Word
import Net.IP
import qualified Chronos.Datetime as CD
import qualified Data.Random      as R
import qualified Data.Text        as T
import qualified Data.Text.IO     as TI
import qualified Net.IPv4         as I4

-- |
genLog :: Text -> FilePath -> IO ()
genLog t f = TI.writeFile f t

parseApache :: Parser Apache
parseApache = 
     (string "apache -a" >> return ApacheAccess)
 <|> (string "apache -e" >> return ApacheError)
 <|> (string "apache -r" >> return ApacheResource)

-- Logs
data Apache = ApacheAccess | ApacheError | ApacheResource

data ApacheAccess = Success | Failure

data ApacheError = General | Startup | Shutdown

data ApacheResource = NoResource

--data PaloAlto = undefined

--data Syslog = undefined
