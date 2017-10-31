{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}

module Silvi.Types 
  ( HttpMethod
  , HttpStatus
  , HttpProtocol(..)
  , HttpProtocolVersion
  , Url
  , Log(..)
  , NcsaLog(..)
  , IPv4(..)
  , fromOctets
  , Word8
  ) where

import Chronos.Types (Offset(..), OffsetDatetime(..))
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

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

newtype Log a = Log { fromLog :: a } deriving (Show, Functor)

type family LogFormat (m :: * -> *) where
  LogFormat (ExceptT  e m) = LogFormat m
  LogFormat (ListT      m) = LogFormat m
  LogFormat (MaybeT     m) = LogFormat m
  LogFormat (ReaderT  r m) = LogFormat m
  LogFormat (RWST r w s m) = LogFormat m
  LogFormat (StateT   s m) = LogFormat m
  LogFormat (WriterT  w m) = LogFormat m

data LogLevel = Debug     -- ^ Debug logs
              | Info      -- ^ Information
              | Notice    -- ^ Normal runtime conditions
              | Warning   -- ^ General warning(s)
              | Error     -- ^ General error(s)
              | Critical  -- ^ Severe situation(s)
              | Alert     -- ^ Take immediate action
              | Panic     -- ^ System is unusable
              | Other     -- ^ Other
              deriving (Eq, Show, Read)


