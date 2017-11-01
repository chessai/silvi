{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}

module Silvi.Types 
  ( HttpMethod(..)
  , HttpStatus(..)
  , HttpProtocol(..)
  , HttpProtocolVersion(..)
  , Url(..)
  , UserId(..)
  , ObjSize(..)
  , NcsaLog(..)
  , IPv4(..)
  , fromOctets
  , Word8
  , OffsetDatetime(..)
  ) where

import Chronos.Types (Offset(..), OffsetDatetime(..))

import Data.Text (Text)
import Data.Word (Word8)
import Net.IPv4 (fromOctets)
import Net.Types (IPv4(..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version

-- | Type alias for Netowrk.HTTP.Types.Method, renamed
--   to be more explicit.
--
type HttpMethod = Method

-- | Type alias for Network.HTTP.Types.Status, renamed 
--   to be more explicit.
--
type HttpStatus = Status

-- | The HTTP Protocol used.
--
data HttpProtocol = HTTP | HTTPS | FTP deriving (Show, Eq)

-- | Type alias for Network.HTTP.Types.Version, renamed
--   to be more explicit.
--
type HttpProtocolVersion = HttpVersion

-- | Url type. 
--   TODO: Expand on this for better randomisation.
--
newtype Url = Url { getUrl :: Text }
  deriving (Show,Eq)

-- | UserId type.
--
newtype UserId = UserId { getUserId :: Text }
  deriving (Show,Eq)

-- | Requested resource size.
--   
newtype ObjSize = ObjSize { getObjSize :: Text }
  deriving (Show,Eq)

-- | Type alias for Common Log
type ApacheLog = NcsaLog

-- | A single log entry from NCSA Common or Extended-formatted log. See: 
--   http://publib.boulder.ibm.com/tividd/td/ITWSA/ITWSA_info45/en_US/HTML/guide/c-logs.html#combined
data NcsaLog = NcsaLog {
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
--  , referrer  :: Maybe Text
--  , userAgent :: Maybe Text
--  , browser   :: Maybe UAResult
--  , platform  :: Maybe OSResult
} deriving (Show)

--data SysLog = SysLog {
--    timestamp :: OffsetDatetime
--  , loghost   :: UserId
--  , app       :: Text
--  , pid       :: Int
--  , syslogid  :: Int
--  , facility  :: Text
--  , level     :: LogLevel
--  , msgId     :: Text
--  , msg       :: Text
--}
