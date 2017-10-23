{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  ( parseIPv4
  , parseTime
  , parseHttpMethod
  --, parseHttpStatus
  , parseHttpProtocolVersion
  ) where

import Silvi.Types
  ( HttpMethod
  , HttpStatus
  , HttpProtocol(..)
  , HttpProtocolVersion
  , Url
  , LogEntry
  , Log
  )

import Chronos (parser_DmyHMSz)
import Chronos.Types 
  ( OffsetDatetime(..)
  , OffsetFormat(..)
  , DatetimeFormat(..))
import Control.Applicative
import Data.Attoparsec.Text 
  ( asciiCI
  , decimal
  , Parser)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Word
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import qualified Data.Attoparsec.Text as Atto

colon, dash, leftBracket, period, quote, rightBracket, slash, space :: Parser Char
colon        = Atto.char ':'
dash         = Atto.char '-'
leftBracket  = Atto.char '['
period       = Atto.char '.'
quote        = Atto.char '"'
rightBracket = Atto.char ']'
slash        = Atto.char '/'
space        = Atto.char ' '

parseIPv4 :: Parser IPv4
parseIPv4 = fmap fromOctets
      (decimal <* period)
  <*> (decimal <* period)
  <*> (decimal <* period)
  <*> decimal
  
--[dd/mm/yyyy:hh:mm:ss -zzzz]
parseTime :: Parser OffsetDatetime
parseTime = do
  leftBracket
  t <- parser_DmyHMSz (offsetFormat) (datetimeFormat)
  rightBracket
  pure t
    where
      offsetFormat = OffsetFormatColonOff
      datetimeFormat = DatetimeFormat (Just '/') (Just ':') (Just ':')

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

--parseHttpStatus :: Parser (Maybe HttpStatus)
--parseHttpStatus = (toEnum . validate) <$> decimal
--  where
--    validate d = if (d >= 200 && d < 506) then Just d else Nothing

parseHttpProtocol :: Parser HttpProtocol
parseHttpProtocol = do
      (asciiCI "HTTPS" *> pure HTTPS)
  <|> (asciiCI "HTTP"  *> pure HTTP )
  <|> (asciiCI "FTP"   *> pure FTP  )
  <|> fail "Invalid HTTP Protocol"

parseHttpProtocolVersion :: Parser HttpProtocolVersion
parseHttpProtocolVersion = do
  pv <- decimal
  period
  sv <- decimal
  pure $ HttpVersion pv sv
