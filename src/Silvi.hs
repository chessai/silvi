{-# LANGUAGE OverloadedStrings #-}

module Silvi
 ( genLog
 ) where

import Data.Text (Text)
import Data.Text.IO
import Silvi.Parsers
import Silvi.Random
import Silvi.Types
import qualified Data.Text        as T

genLog :: Text -> Text
genLog _ = "hello world!"

main :: IO ()
main = do
  print "hello world!"
