{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  ( parseIPv4
  , parseOffsetDatetime
  , parseHttpMethod
  , parseHttpProtocol
  , parseHttpProtocolVersion
  , parseHttpStatus
  ) where

import Silvi.Types

import Chronos
import Chronos.Types 
  ( OffsetDatetime(..)
  , OffsetFormat(..)
  , DatetimeFormat(..))
import Control.Applicative
import Data.Attoparsec.Text 
  ( asciiCI
  , decimal
  , Parser
  , takeTill)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Text (Text)
import Data.Word
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T

parseLogEntry :: Parser LogEntry
parseLogEntry = LogEntry 
  <$> parseIPv4
  <*> (space *> parseUserident)
  <*> (space *> parseUserident)
  <*> (space *> parseOffsetDatetime)
  <*> (space *> quote *> parseHttpMethod)
  <*> (space *> parseUrl)
  <*> (space *> parseHttpProtocol)
  <*> (space *> parseHttpProtocolVersion <* quote)
  <*> (space *> parseHttpStatus)
  <*> (space *> parseObjSize)


parseIPv4 :: Parser IPv4
parseIPv4 = fromOctets
  <$> (decimal <* period)
  <*> (decimal <* period)
  <*> (decimal <* period)
  <*> decimal
 
parseHttpMethod :: Parser HttpMethod
parseHttpMethod = do
      (asciiCI "GET"     *> pure methodGet    )
  <|> (asciiCI "POST"    *> pure methodPost   ) 
  <|> (asciiCI "HEAD"    *> pure methodHead   )
  <|> (asciiCI "PUT"     *> pure methodPut    )
  <|> (asciiCI "DELETE"  *> pure methodDelete )
  <|> (asciiCI "TRACE"   *> pure methodTrace  )
  <|> (asciiCI "CONNECT" *> pure methodConnect)
  <|> (asciiCI "OPTIONS" *> pure methodOptions)
  <|> (asciiCI "PATCH"   *> pure methodPatch  )
  <|> fail "Invalid HTTP Method"

parseHttpStatus :: Parser HttpStatus
parseHttpStatus = toEnum <$> decimal

parseHttpProtocol :: Parser HttpProtocol
parseHttpProtocol = 
      (asciiCI "HTTPS" *> pure HTTPS)
  <|> (asciiCI "HTTP"  *> pure HTTP )
  <|> (asciiCI "FTP"   *> pure FTP  )
  <|> fail "Invalid HTTP Protocol"

parseHttpProtocolVersion :: Parser HttpProtocolVersion
parseHttpProtocolVersion = HttpVersion
  <$> decimal 
  <*> (period *> decimal)

parseUserident :: Parser (Maybe Text)
parseUserident = do
  ident <- takeTill (== ' ')
  pure $ if ((T.length ident) == 1) && (T.head ident) == '-' then Nothing else Just ident

parseObjSize :: Parser Int
parseObjSize = decimal <* space

parseQuote :: Parser (Maybe Text)
parseQuote = fmap refTest (quote *> takeTill (== '"') <* quote)
  where refTest r = if (T.length r == 0) then Nothing else Just r

parseUrl :: Parser Text
parseUrl = takeTill (== ' ')

--[dd/mm/yyyy:hh:mm:ss -zzzz]
parseOffsetDatetime :: Parser OffsetDatetime
parseOffsetDatetime = do
  leftBracket
  odt <- parser_DmyHMSz (offsetFormat) (datetimeFormat)
  rightBracket
  pure odt
  where offsetFormat = OffsetFormatColonOff
        datetimeFormat = DatetimeFormat (Just '/') (Just ':') (Just ':')
