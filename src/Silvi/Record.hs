{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Silvi.Record 
  ( -- * Record stuff
    rmap
  , Field(..)
  , SingField(..)

    -- * unit Log types
 -- , HttpMethod(..)
 --  , HttpStatus(..)
 --  , HttpProtocol(..)
 --  , HttpProtocolVersion(..)
 --  , Url(..)
 --  , UserId(..)
 --  , ObjSize(..)
 --  , IPv4(..)
 -- , OffsetDatetime(..)
  ) where

import           Chronos.Types 
  ( Offset(..)
  , OffsetDatetime(..)
  )
import           Data.Exists
  ( Reify(..)
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
import           Topaz.Rec (Rec(..), traverse)
import qualified Topaz.Rec as Topaz

-- | Alias for `Topaz.map`.
rmap :: (forall x. f x -> g x) -> Rec f as -> Rec g as
rmap = Topaz.map

-- | Different types present in logs.
data Field
  = HttpMethod          -- ^ More explicit name for Network.HTTP.Types.Method
  | HttpStatus          -- ^ More explicit name for Network.HTTP.Types.Status
  | HttpProtocol        -- ^ The HTTP Protocol used
  | HttpProtocolVersion -- ^ More explicit name for Network.HTTP.Types.Version
  | Url                 -- ^ a url, e.g. "https://hackage.haskell.org"
  | UserId              -- ^ userId as Text
  | ObjSize             -- ^ usually requested resource size
  | Ip                  -- ^ Ip present in log
  | Timestamp           -- ^ Timestamp
  deriving (Bounded,Enum,Eq,Generic,Ord,Read,Show)

--type NcsaLog = '[ Ip
--                , UserId
--                , Timestamp
--                , HttpMethod
--                , Url
--                , HttpProtocol
--                , HttpProtocolVersion
--                , HttpStatus
--                , ObjSize
--                ]

data SingField (v :: Field) where
  SingHttpMethod          :: SingField 'HttpMethod
  SingHttpStatus          :: SingField 'HttpStatus
  SingHttpProtocol        :: SingField 'HttpProtocol
  SingHttpProtocolVersion :: SingField 'HttpProtocolVersion
  SingUrl                 :: SingField 'Url
  SingUserId              :: SingField 'UserId
  SingObjSize             :: SingField 'ObjSize
  SingIp                  :: SingField 'Ip
  SingTimestamp           :: SingField 'Timestamp

type instance Sing = SingField

instance Reify 'HttpMethod where
  reify = SingHttpMethod
instance Reify 'HttpStatus where
  reify = SingHttpStatus
instance Reify 'HttpProtocol where
  reify = SingHttpProtocol
instance Reify 'HttpProtocolVersion where
  reify = SingHttpProtocolVersion
instance Reify 'Url where
  reify = SingUrl
instance Reify 'UserId where
  reify = SingUserId
instance Reify 'ObjSize where
  reify = SingObjSize
instance Reify 'Ip where
  reify = SingIp
instance Reify 'Timestamp where
  reify = SingTimestamp

instance Unreify Field where
  unreify SingHttpMethod x = x
  unreify SingHttpStatus x = x
  unreify SingHttpProtocol x = x
  unreify SingHttpProtocolVersion x = x
  unreify SingUrl x = x
  unreify SingUserId x = x
  unreify SingObjSize x = x
  unreify SingIp x = x
  unreify SingTimestamp x = x

data HttpProtocol = HTTP | HTTPS | FTP 
  deriving (Eq,Show)

