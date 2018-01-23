{-# LANGUAGE GADTs #-}

module Silvi.Types
  ( Url(..)
  , UserId(..)
  , ObjSize(..)
  , BracketNum(..)
  ) where

{-# OPTIONS_GHC -Wall #-}

import           Data.Exists
import           Data.Text (Text)
import           Data.Word (Word16, Word8)
import           Topaz.Rec (Rec(..))

-- | Url type.
newtype Url = Url { getUrl :: Text }
  deriving (Eq, Show)

-- | UserId type.
newtype UserId = UserId { getUserId :: Text }
  deriving (Eq, Show)

-- | Requested resource size.
newtype ObjSize = ObjSize { getObjSize :: Word16 }
  deriving (Eq, Show)

-- | Angle-bracketed number that appears before some logs.
newtype BracketNum = BracketNum { getBracketNum :: Word8 }
  deriving (Eq, Show)
