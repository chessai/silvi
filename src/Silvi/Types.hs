{-# LANGUAGE OverloadedStrings #-}

module Silvi.Types 
  ( Time(..)
  , UserIdent
  , UserId
  , Request
  , Resource
  , HttpProtocol
  , HttpStatusCode
  , ObjectSize
  ) where

import Chronos.Types (Datetime)
import Data.Text
import Net.Types

type   RFC1413        = Text
data    Time           = Time { datetime :: Datetime, zone :: Text } deriving (Bounded, Enum, Show)
newtype UserIdent      = UserIdent { getUserIdent :: RFC1413 }
newtype UserId         = UserId { getUserId :: Text }
newtype Request        = Request { getRequest :: Text }
newtype Resource       = Resource { getResource :: Text }
newtype HttpProtocol   = HttpProtocol { getHttpProtocol :: Text }
newtype HttpStatusCode = HttpStatusCode { getHttpStatusCode :: Int }
newtype ObjectSize     = ObjectSize { getObjectSize :: Integer}
