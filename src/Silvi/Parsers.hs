{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  ( parseIPv4
  , parseTime
  ) where

import Silvi.Types

import Chronos.OffsetDatetime.Text (parser_DmyHMSz)
import Chronos.Types (DatetimeFormat(..), OffsetDatetime(..), OffsetFormat(..) )
import Control.Applicative
import Data.Attoparsec.Text (decimal, Parser)
import Data.Foldable (traverse_)
import Data.Word
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
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

--[dd/mm/yyyy:hh:mm:ss -zzzz]
parseTime :: Parser OffsetDatetime
parseTime = do
  traverse_ Atto.char (Just '[')
  t <- parser_DmyHMSz (fst clfTime) (snd clfTime)
  traverse_ Atto.char (Just ']')
  pure t
  
