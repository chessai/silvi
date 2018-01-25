{-# LANGUAGE GADTs #-}

module Silvi.Internal.Types
  ( Url(..)
  , UserId(..)
  , ObjSize(..)
  , BracketNum(..)
  , Silvi 
  ) where

{-# OPTIONS_GHC -Wall #-}

import           Data.Exists
import           Data.Text (Text)
import           Data.Word (Word16, Word8)
import           Savage
import           Topaz.Rec (Rec(..))
import           Silvi.Record (Field(..), Value(..), SingField(..), Rec(..))

type Silvi a = Gen (Rec Value a)

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
