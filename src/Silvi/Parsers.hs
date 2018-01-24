{-# LANGUAGE OverloadedStrings #-}

module Silvi.Parsers 
  ( parseIPv4
  , parseIPv6
  , parseHttpMethod
  ) where

{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Data.Attoparsec.Text (asciiCI, decimal, takeTill, Parser) 
import Data.Text (Text)
import Net.Types (IPv4, IPv6)
import qualified Net.IPv4 as I4
import qualified Net.IPv6 as I6
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
parseIPv4 = I4.parser

parseIPv6 :: Parser IPv6
parseIPv6 = I6.parser

parseHttpMethod :: Parser HttpM.Method
parseHttpMethod = do
      (asciiCI "GET"     *> pure HttpM.methodGet    )
  <|> (asciiCI "POST"    *> pure HttpM.methodPost   ) 
  <|> (asciiCI "HEAD"    *> pure HttpM.methodHead   )
  <|> (asciiCI "PUT"     *> pure HttpM.methodPut    )
  <|> (asciiCI "DELETE"  *> pure HttpM.methodDelete )
  <|> (asciiCI "TRACE"   *> pure HttpM.methodTrace  )
  <|> (asciiCI "CONNECT" *> pure HttpM.methodConnect)
  <|> (asciiCI "OPTIONS" *> pure HttpM.methodOptions)
  <|> (asciiCI "PATCH"   *> pure HttpM.methodPatch  )
  <|> fail "Invalid HTTP Method"

parseHttpStatus :: Parser HttpS.Status
parseHttpStatus = toEnum <$> decimal

{-
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
-}
