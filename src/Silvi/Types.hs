{-# LANGUAGE OverloadedStrings #-}

module Silvi.Types 
  ( HttpMethod
  , HttpStatus
  , HttpProtocol(..)
  , HttpProtocolVersion
  , Url
  , LogEntry(..)
  , Log
  , colon
  , dash
  , dot
  , fullStop
  , leftBracket
  , period
  , quote
  , rightBracket
  , slash
  , space
  ) where

import Chronos.Types (Offset(..), OffsetDatetime(..))
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Net.IPv4 (fromOctets)
import Net.Types (IPv4(..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Web.UAParser
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Lazy.Char8 as BC

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

-- | Type alias for Netowrk.HTTP.Types.Method, renamed
--   to be more explicit.
type HttpMethod = Method

-- | Type alias for Network.HTTP.Types.Status, renamed 
--   to be more explicit.
type HttpStatus = Status

-- | The HTTP Protocol used. This should be in http-types.
data HttpProtocol = HTTP | HTTPS | FTP deriving (Show, Eq)

-- | Type alias for Network.HTTP.Types.Version, renamed
--   to be more explicit.
type HttpProtocolVersion = HttpVersion

-- | Type alias
newtype Url = Url { getUrl :: Text }
  deriving (Show,Eq)

-- | A single log entry from NCSA Common or Extended-formatted log. See: 
--   http://publib.boulder.ibm.com/tividd/td/ITWSA/ITWSA_info45/en_US/HTML/guide/c-logs.html#combined
data LogEntry = LogEntry {
    ip        :: IPv4
  , identity  :: Maybe Text
  , userid    :: Maybe Text
  , timestamp :: OffsetDatetime
  , method    :: HttpMethod
  , url       :: Text
  , protocol  :: HttpProtocol
  , protovers :: HttpProtocolVersion
  , status    :: HttpStatus
  , objSize   :: Int
  --, referrer  :: Maybe Text
  --, userAgent :: Maybe Text
  --, browser   :: Maybe UAResult
  --, platform  :: Maybe OSResult
} deriving (Show)

type Log = [LogEntry]
