{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Silvi.Random
  ( rand
  , randLogExplicit
  , randLog
  ) where

import           Chronos.Types
import           Data.Exists                (Exists (..), Reify (..),
                                             SingList (..))
import           Data.Text                  (Text)
import           Data.Word                  (Word8)
import           Net.IPv4                   (ipv4)
import           Net.Types                  (IPv4(..))
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Version
import           Savage
import           Savage.Randy               (element, enum, enumBounded, int, print, word8)
import           Savage.Range               (constantBounded)
import           Silvi.Record               (Field (..), SingField (..),
                                             Value (..), rmap, rtraverse)
import           Silvi.Types
import           Topaz.Rec                  (Rec (..), fromSingList)

rand :: SingField a -> Gen (Value a)
rand = \case
  SingHttpMethod  -> fmap ValueHttpMethod randomHttpMethod
  SingHttpStatus  -> fmap ValueHttpStatus randomHttpStatus
  SingHttpVersion -> fmap ValueHttpVersion randomHttpVersion
  SingUrl         -> fmap ValueUrl randomUrl
  SingUserId      -> fmap ValueUserId randomUserident
  SingObjSize     -> fmap ValueObjSize randomObjSize
  SingIp          -> fmap ValueIp randomIPv4
  SingTimestamp   -> fmap ValueTimestamp randomOffsetDatetime

randLog :: forall as. (Reify as) => Gen (Rec Value as)
randLog = randLogExplicit (fromSingList (reify :: SingList as))

randLogExplicit :: Rec SingField rs -> Gen (Rec Value rs)
randLogExplicit = rtraverse rand

randomIPv4 :: Gen IPv4
randomIPv4 = ipv4
  <$> word8 constantBounded
  <*> word8 constantBounded
  <*> word8 constantBounded
  <*> word8 constantBounded

randomHttpMethod :: Gen StdMethod
randomHttpMethod = enumBounded

randomHttpStatus :: Gen HttpStatus
randomHttpStatus = enumBounded

randomHttpVersion :: Gen HttpVersion
randomHttpVersion = enumBounded

randomUserident :: Gen Text
randomUserident = element userIdents

randomObjSize :: Gen Int
randomObjSize = int constantBounded

randomQuote :: Gen Text
randomQuote = element quotes

randomUrl :: Gen Text
randomUrl = element urls

randomDatetime :: Gen Datetime
randomDatetime = Datetime
  <$> randomDate
  <*> randomTimeOfDay

randomTimeOfDay :: Gen TimeOfDay
randomTimeOfDay = TimeOfDay
  <$> enum 0    24
  <*> enum 0    59
  <*> enum 0 59999

randomDate :: Gen Date
randomDate = Date
  <$> (Year       <$> enum 1995 2100)
  <*> (Month      <$> enum    0   11)
  <*> (DayOfMonth <$> enum    1   31)


randomOffsetDatetime :: Gen OffsetDatetime
randomOffsetDatetime = OffsetDatetime
  <$> randomDatetime
  <*> randomOffset

randomOffset :: Gen Offset
randomOffset = element offsets

-- | List of sample Useridents.
userIdents :: [Text]
userIdents = ["-","andrewthad","cement","chessai"]

-- | List of sample URLs.
urls :: [Text]
urls = ["https://github.com","https://youtube.com","layer3com.com"]

quotes :: [Text]
quotes = ["customerA got caught putting their hand in the cookie jar","customerB uses firefox instead of chrome"]

-- | List of sample
-- | List of Time Zone Offsets. See:
--   https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
offsets :: [Offset]
offsets = map Offset [100,200,300,330,400,430,500,530,545,600,630,700,800,845,900,930,1000,1030,1100,1200,1245,1300,1345,1400,0,-100,-200,-230,-300,-330,-400,-500,-600,-700,-800,-900,-930,-1000,-1100,-1200]
