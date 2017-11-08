{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Silvi.Record 
  ( rmap
  , NcsaLog
  , TestLog
  , Field(..)
  , Value(..)
  , SingField(..)
  , rtraverse
  ) where

import           Chronos.Types 
  ( Offset(..)
  , OffsetDatetime(..)
  )
import           Data.Exists
  ( Exists(..)
  , Reify(..)
  , Sing
  , Unreify(..)
  )
import           Data.Kind (Type)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Net.Types (IPv4)
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Version
import           Silvi.Types
import           Topaz.Rec (Rec(..), traverse)
import qualified Topaz.Rec as Topaz

-- | Different types present in logs.
data Field
  = FieldHttpMethod          -- ^ More explicit name for Network.HTTP.Types.Method
  | FieldHttpStatus          -- ^ More explicit name for Network.HTTP.Types.Status
  | FieldHttpProtocol        -- ^ The HTTP Protocol used
  | FieldHttpProtocolVersion -- ^ More explicit name for Network.HTTP.Types.Version
  | FieldUrl                 -- ^ a url, e.g. "https://hackage.haskell.org"
  | FieldUserId              -- ^ userId as Text
  | FieldObjSize             -- ^ usually requested resource size
  | FieldIp                  -- ^ FieldIp present in log
  | FieldTimestamp           -- ^ Timestamp
  deriving (Bounded,Enum,Eq,Generic,Ord,Read,Show)

data Value :: Field -> Type where
  ValueHttpMethod :: HttpMethod -> Value 'FieldHttpMethod
  ValueHttpStatus :: HttpStatus -> Value 'FieldHttpStatus
  ValueHttpProtocol :: HttpProtocol -> Value 'FieldHttpProtocol
  ValueHttpProtocolVersion :: HttpProtocolVersion -> Value 'FieldHttpProtocolVersion
  ValueUrl :: Text -> Value 'FieldUrl
  ValueUserId :: Text -> Value 'FieldUserId
  ValueObjSize :: Int -> Value 'FieldObjSize
  ValueIp :: IPv4 -> Value 'FieldIp
  ValueTimestamp :: OffsetDatetime -> Value 'FieldTimestamp

data SingField :: Field -> Type where
  SingHttpMethod          :: SingField 'FieldHttpMethod
  SingHttpStatus          :: SingField 'FieldHttpStatus
  SingHttpProtocol        :: SingField 'FieldHttpProtocol
  SingHttpProtocolVersion :: SingField 'FieldHttpProtocolVersion
  SingUrl                 :: SingField 'FieldUrl
  SingUserId              :: SingField 'FieldUserId
  SingObjSize             :: SingField 'FieldObjSize
  SingIp                  :: SingField 'FieldIp
  SingTimestamp           :: SingField 'FieldTimestamp


type TestLog = '[ FieldIp
                , FieldTimestamp
                ]

type NcsaLog = '[ FieldIp
                , FieldUserId
                , FieldTimestamp
                , FieldHttpMethod
                , FieldUrl
                , FieldHttpProtocol
                , FieldHttpProtocolVersion
                , FieldHttpStatus
                , FieldObjSize
                ]

type instance Sing = SingField
instance Reify 'FieldHttpMethod where
  reify = SingHttpMethod
instance Reify 'FieldHttpStatus where
  reify = SingHttpStatus
instance Reify 'FieldHttpProtocol where
  reify = SingHttpProtocol
instance Reify 'FieldHttpProtocolVersion where
  reify = SingHttpProtocolVersion
instance Reify 'FieldUrl where
  reify = SingUrl
instance Reify 'FieldUserId where
  reify = SingUserId
instance Reify 'FieldObjSize where
  reify = SingObjSize
instance Reify 'FieldIp where
  reify = SingIp
instance Reify 'FieldTimestamp where
  reify = SingTimestamp

-- | Alias for `Topaz.map`.
rmap :: (forall x. f x -> g x) -> Rec f as -> Rec g as
rmap = Topaz.map

-- | Alias for `Topaz.traverse`
rtraverse
  :: Applicative h
  => (forall x. f x -> h (g x))
  -> Rec f rs
  -> h (Rec g rs)
rtraverse = Topaz.traverse
