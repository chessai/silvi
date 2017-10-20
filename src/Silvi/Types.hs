{-# LANGUAGE OverloadedStrings #-}

module Silvi.Types 
  ( clfTime
  , UserIdent
  , UserId
  , Request
  , Resource
  , ObjectSize
  ) where

import Chronos.Types (OffsetDatetime(..), OffsetFormat(..), DatetimeFormat(..))
import Data.Text
import Net.Types
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version 

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
