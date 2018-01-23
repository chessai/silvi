{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  (
  ) where

import Silvi.Types

import qualified Chronos
import           Chronos.Types 
  ( OffsetDatetime(..)
  , OffsetFormat(..)
  , DatetimeFormat(..))
import Control.Applicative
import Data.Attoparsec.Text 
  ( asciiCI
  , decimal
  , Parser
  , takeTill)
import Data.Maybe
import Data.Text (Text)
import Net.IPv4 (ipv4)
import qualified Network.HTTP.Types.Method as HttpM
import qualified Network.HTTP.Types.Status as HttpS
import qualified Network.HTTP.Types.Version as HttpV
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T

-- | Useful aliases for parsing
colon, dash, dot, fullStop, leftBracket, period, quote, rightBracket, slash, space :: Parser Char
colon        = Atto.char ':'
dash         = Atto.char '-'
dot          = period
fullStop     = period
leftBracket  = Atto.char '['
period       = Atto.char '.'
quote        = Atto.char '"'
rightBracket = Atto.char ']'
slash        = Atto.char '/'
space        = Atto.char ' '

parseIPv4 :: Parser IPv4
parseIPv4 = ipv4
  <$> (decimal <* period)
  <*> (decimal <* period)
  <*> (decimal <* period)
  <*> decimal
 
parseHttpMethod :: Parser HttpM.StdMethod
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
