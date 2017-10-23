{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  ( parseIPv4
  , parseTime
  , parseHttpMethod
  , parseHttpStatus
  , parseHttpVersion
  ) where

import Silvi.Types

import Chronos (parser_DmyHMSz)
import Chronos.Types ( OffsetDatetime(..) )
import Control.Applicative
import Data.Attoparsec.Text (decimal, Parser)
import Data.Foldable (traverse_)
import Data.Word
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
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

parseHttpMethod :: Parser HttpMethod
parseHttpMethod = do
      ("GET"     *> pure methodGet)
  <|> ("POST"    *> pure methodPost)
  <|> ("PUT"     *> pure methodPut)
  <|> ("DELETE"  *> pure methodDelete)
  <|> ("OPTIONS" *> pure methodOptions)
  <|> ("HEAD"    *> pure methodHead)
  <|> ("TRACE"   *> pure methodTrace)
  <|> ("CONNECT" *> pure methodConnect)
  <|> ("PATCH"   *> pure methodPatch)

parseHttpStatus :: Parser HttpStatus
parseHttpStatus = do
  s <- decimal
  pure $ toEnum s

parseHttpVersion :: Parser HttpVersion
parseHttpVersion = do
  "HTTP/" *> pure ()
  pv <- decimal
  traverse_ Atto.char (Just '.')
  sv <- decimal
  pure $ HttpVersion pv sv
