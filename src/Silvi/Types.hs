{-# LANGUAGE OverloadedStrings #-}

module Silvi.Types 
  ( HttpMethod
  , HttpStatus
  , HttpProtocol(..)
  , HttpProtocolVersion
  , Url
  , LogEntry
  , Log
  , httpMethods
  , httpStatuses
  , httpProtocolVersions
  , offsets
  ) where

import Chronos.Types (Offset(..), OffsetDatetime(..))
import Net.Types (IPv4(..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Web.UAParser
import qualified Data.ByteString.Lazy.Char8 as BC

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
type Url = BC.ByteString

-- | A single log entry from NCSA Common or Extended-formatted log. See: 
--   http://publib.boulder.ibm.com/tividd/td/ITWSA/ITWSA_info45/en_US/HTML/guide/c-logs.html#combined
data LogEntry = LogEntry {
    ip        :: IPv4
  , identity  :: Maybe BC.ByteString
  , userid    :: Maybe BC.ByteString
  , timestamp :: OffsetDatetime
  , method    :: Maybe HttpMethod
  , url       :: Url
  , protocol  :: Maybe HttpProtocol
  , status    :: Maybe HttpStatus
  , objSize   :: Int
  , referrer  :: Maybe Url
  , userAgent :: Maybe BC.ByteString
  , browser   :: Maybe UAResult
  , platform  :: Maybe OSResult
} deriving (Show)

type Log = [LogEntry]

-- | List of HTTP Methods.
httpMethods :: [HttpMethod]
httpMethods = [methodGet,methodPost,methodHead,methodPut,methodDelete,methodTrace,methodConnect,methodOptions,methodPatch]

-- | List of HTTP Protocol Versions.
httpProtocolVersions :: [HttpProtocolVersion]
httpProtocolVersions = [http09,http10,http11,http20]

-- | List of HTTP Status Codes.
httpStatuses :: [HttpStatus]
httpStatuses = [status200,status204,status301,status400,status401,status403,status404,status405,status500,status503,status504]

-- | List of Time Zone Offsets. See: 
--   https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
offsets :: [Offset]
offsets = fmap Offset [100,200,300,330,400,430,500,530,545,600,630,700,800,845,900,930,1000,1030,1100,1200,1245,1300,1345,1400,0,(-100),(-200),(-230),(-300),(-330),(-400),(-500),(-600),(-700),(-800),(-900),(-930),(-1000),(-1100),(-1200)]
