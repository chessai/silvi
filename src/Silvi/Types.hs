module Silvi.Types
  ( Url(..)
  , UserId(..)
  , ObjSize(..)
  ) where

import           Data.Text (Text)

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
newtype ObjSize = ObjSize { getObjSize :: Int }
  deriving (Show,Eq)
