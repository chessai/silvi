{-# LANGUAGE OverloadedStrings #-}

module Silvi.Types 
  ( clfTime
  , HttpMethod
  , HttpStatus
  , UserIdent
  , UserId
  , Request
  , Resource
  , ObjectSize
  ) where

import Chronos.Types (OffsetDatetime(..), OffsetFormat(..), DatetimeFormat(..))
import Data.Text
import Net.Types
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version 

-- | Type alias for Netowrk.HTTP.Types.Method, renamed
--   to be more explicit.
type HttpMethod = Method

-- | Type alias for Network.HTTP.Types.Status, renamed 
--   to be more explicit.
type HttpStatus = Status

-- | Common Log Format: https://en.wikipedia.org/wiki/Common_Log_Format#Example
--   This is the format which time follows in CLF.
clfTime :: (OffsetFormat, DatetimeFormat)
clfTime = (OffsetFormatColonOff, DatetimeFormat (Just '/') (Just ':') (Just ':'))

newtype UserIdent = UserIdent { getUserIdent :: Text }
  deriving (Show,Read,Eq,Ord)

newtype UserId = UserId { getUserId :: Text }
  deriving (Show,Read,Eq,Ord)

newtype Request = Request { getRequest :: Text }
  deriving (Show,Read,Eq,Ord)

newtype Resource = Resource { getResource :: Text }
  deriving (Show,Read,Eq,Ord)

newtype ObjectSize = ObjectSize { getObjectSize :: Integer }
  deriving (Show,Read,Eq,Ord)
