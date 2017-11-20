module Silvi.Types
  ( Url(..)
  , UserId(..)
  , ObjSize(..)
  , BracketNum(..)
  ) where

import           Data.Text (Text)
import           Data.Word (Word16, Word8)

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
newtype ObjSize = ObjSize { getObjSize :: Word16 }
  deriving (Show,Eq)

-- | Angle-bracketed number that appears before many logs.
--
newtype BracketNum = BracketNum { getBracketNum :: Word8 }
  deriving (Eq)

instance Show BracketNum where
  show (BracketNum x) = "<" ++ show x ++ ">"
