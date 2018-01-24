{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wall #-}

module Silvi.Record
  ( Field(..)
  , Value(..)
  , SingField(..)
  , Rec(..) 
  ) where

import           Chronos.Types              (OffsetDatetime (..))
import           Data.Exists                (Reify (..),
                                             ShowForall (..), Sing)
import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)
import           Net.Types                  (IPv4,IPv6)
import qualified Network.HTTP.Types.Method  as HttpM
import qualified Network.HTTP.Types.Status  as HttpS
import qualified Network.HTTP.Types.Version as HttpV
import           Silvi.Encode
import           Silvi.Types
import           Topaz.Rec                  (Rec(..))

-- | Different types present in logs.
data Field
  = FieldBracketNum          -- ^ Number that appears before many logs, in the form of "<X>"
  | FieldHttpMethod          -- ^ More explicit name for Network.HTTP.Types.Method
  | FieldHttpStatus          -- ^ More explicit name for Network.HTTP.Types.Status
  | FieldHttpVersion         -- ^ More explicit name for Network.HTTP.Types.Version
  | FieldUrl                 -- ^ a url, e.g. "https://hackage.haskell.org"
  | FieldUserId              -- ^ userId as Text
  | FieldObjSize             -- ^ usually requested resource size
  | FieldIPv4                -- ^ IPv4 present in log
  | FieldIPv6                -- ^ IPv6 address 
  | FieldTimestamp           -- ^ Timestamp 
  deriving (Bounded,Enum,Eq,Generic,Ord,Read,Show)

data Value :: Field -> Type where
  ValueBracketNum  :: BracketNum        -> Value 'FieldBracketNum
  ValueHttpMethod  :: HttpM.StdMethod   -> Value 'FieldHttpMethod
  ValueHttpStatus  :: HttpS.Status      -> Value 'FieldHttpStatus
  ValueHttpVersion :: HttpV.HttpVersion -> Value 'FieldHttpVersion
  ValueUrl         :: Url               -> Value 'FieldUrl
  ValueUserId      :: UserId            -> Value 'FieldUserId
  ValueObjSize     :: ObjSize           -> Value 'FieldObjSize
  ValueIPv4        :: IPv4              -> Value 'FieldIPv4
  ValueIPv6        :: IPv6              -> Value 'FieldIPv6 
  ValueTimestamp   :: OffsetDatetime    -> Value 'FieldTimestamp

instance ShowForall Value where
  showsPrecForall p (ValueBracketNum x)  = showParen (p > 10) $ showString "ValueBracketNum"  . showsPrec 11 x
  showsPrecForall p (ValueHttpMethod x)  = showParen (p > 10) $ showString "ValueHttpMethod"  . showsPrec 11 x
  showsPrecForall p (ValueHttpStatus x)  = showParen (p > 10) $ showString "ValueHttpStatus"  . showsPrec 11 x
  showsPrecForall p (ValueHttpVersion x) = showParen (p > 10) $ showString "ValueHttpVersion" . showsPrec 11 x
  showsPrecForall p (ValueUrl x)         = showParen (p > 10) $ showString "ValueUrl"         . showsPrec 11 x
  showsPrecForall p (ValueUserId x)      = showParen (p > 10) $ showString "ValueUserId"      . showsPrec 11 x
  showsPrecForall p (ValueObjSize x)     = showParen (p > 10) $ showString "ValueObjSize"     . showsPrec 11 x
  showsPrecForall p (ValueIPv4 x)        = showParen (p > 10) $ showString "ValueIPv4"        . showsPrec 11 x
  showsPrecForall p (ValueIPv6 x)        = showParen (p > 10) $ showString "ValueIPv6"        . showsPrec 11 x 
  showsPrecForall p (ValueTimestamp x)   = showParen (p > 10) $ showString "ValueTimestamp"   . showsPrec 11 x

data SingField :: Field -> Type where
  SingBracketNum  :: SingField 'FieldBracketNum
  SingHttpMethod  :: SingField 'FieldHttpMethod
  SingHttpStatus  :: SingField 'FieldHttpStatus
  SingHttpVersion :: SingField 'FieldHttpVersion
  SingUrl         :: SingField 'FieldUrl
  SingUserId      :: SingField 'FieldUserId
  SingObjSize     :: SingField 'FieldObjSize
  SingIPv4        :: SingField 'FieldIPv4
  SingIPv6        :: SingField 'FieldIPv6 
  SingTimestamp   :: SingField 'FieldTimestamp

type instance Sing = SingField

instance Reify 'FieldBracketNum where
  reify = SingBracketNum
instance Reify 'FieldHttpMethod where
  reify = SingHttpMethod
instance Reify 'FieldHttpStatus where
  reify = SingHttpStatus
instance Reify 'FieldHttpVersion where
  reify = SingHttpVersion
instance Reify 'FieldUrl where
  reify = SingUrl
instance Reify 'FieldUserId where
  reify = SingUserId
instance Reify 'FieldObjSize where
  reify = SingObjSize
instance Reify 'FieldIPv4 where
  reify = SingIPv4
instance Reify 'FieldIPv6 where
  reify = SingIPv6
instance Reify 'FieldTimestamp where
  reify = SingTimestamp

instance Encode (Value a) where
  encode = \case
    ValueBracketNum  x -> encode x
    ValueHttpMethod  x -> encode x
    ValueHttpStatus  x -> encode x
    ValueHttpVersion x -> encode x 
    ValueUrl         x -> encode x
    ValueUserId      x -> encode x
    ValueObjSize     x -> encode x
    ValueIPv4        x -> encode x
    ValueIPv6        x -> encode x
    ValueTimestamp   x -> encode x 
