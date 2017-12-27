{-# LANGUAGE GADTs #-}

module Silvi.Types
  ( Url(..)
  , UserId(..)
  , ObjSize(..)
  , BracketNum(..)
  , Log(..) 
  ) where

import           Data.Exists
import           Data.Text (Text)
import           Data.Word (Word16, Word8)
import           Topaz.Rec (Rec(..))

newtype Log v a = Log { getLog :: Rec v a }
  deriving (Eq)

instance ShowForall f => ShowForall (Log f) where
  showsPrecForall p x = case getLog x of
    RecCons v vs -> showParen (p > 10)
      $ showsPrecForall 11 v
      . showString " "
      . showsPrecForall 11 vs
    RecNil  -> showString ""

-- | Url type.
--   TODO: Expand on this for better randomisation.
--
newtype Url = Url { getUrl :: Text }
  deriving (Eq)

instance Show Url where
  show (Url x) = show x

-- | UserId type.
--
newtype UserId = UserId { getUserId :: Text }
  deriving (Eq)

instance Show UserId where
  show (UserId x) = show x

-- | Requested resource size.
--
newtype ObjSize = ObjSize { getObjSize :: Word16 }
  deriving (Eq)

instance Show ObjSize where
  show (ObjSize x) = show x ++ "B"

-- | Angle-bracketed number that appears before many logs.
--
newtype BracketNum = BracketNum { getBracketNum :: Word8 }
  deriving (Eq)

instance Show BracketNum where
  show (BracketNum x) = "<" ++ show x ++ ">"
