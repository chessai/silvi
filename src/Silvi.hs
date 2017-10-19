{-# LANGUAGE OverloadedStrings #-}

module Silvi
 ( genLog
 ) where

import Silvi.Parsers
import Silvi.Random
import Silvi.Types

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Data.Text.IO
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text            as T

genLog :: Text -> Text
genLog _ = "hello world!"

main :: IO ()
main = do
  
  print $ parseOnly parseIPv4 "131.45.68.123"
  print $ parseOnly parseTime "[08/08/1996:12:44:00 -0600]"

--[dd/mm/yyyy:hh:mm:ss -zzzz]
