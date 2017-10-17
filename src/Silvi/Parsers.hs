{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  ( parseIPv4
  ) where

import Chronos.Datetime.Text --(parser_DmyHMS)
import Chronos.Types (DatetimeFormat)
import Control.Applicative
import Data.Attoparsec.Text (decimal, Parser)
import Data.Word
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Silvi.Types
import qualified Data.Attoparsec.Text as Atto

parseIPv4 :: Parser IPv4
parseIPv4 = do
  d1 <- decimal
  Atto.char '.'
  d2 <- decimal
  Atto.char '.'
  d3 <- decimal
  Atto.char '.'
  d4 <- decimal
  pure $ fromOctets d1 d2 d3 d4

-- |This will not work until the next version of Chronos.
--parseTime :: Parser Time
--parseTime = do
--  traverse_ Atto.char '['
--  datetime <- parser_DmyHMS (DatetimeFormat '/' ':' ':')
--  traverse_ Atto.char ' '
--  zone <- Atto.take 5 
--  traverse_ Atto.char ']'
--  pure $ (Time datetime zone)
  
