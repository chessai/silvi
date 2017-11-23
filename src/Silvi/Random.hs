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

import           Chronos                    (now, timeToDatetime)
import           Chronos.Types
import           Data.Exists                (Exists (..), Reify (..),
                                             SingList (..))
import           Data.Text                  (Text)
import           Data.Word                  (Word8)
import           Net.IPv4                   (ipv4)
import           Net.Types                  (IPv4 (..))
import qualified Network.HTTP.Types.Method  as HttpM
import qualified Network.HTTP.Types.Status  as HttpS
import           Network.HTTP.Types.Version (http09, http10, http11, http20)
import qualified Network.HTTP.Types.Version as HttpV
import           Savage
import           Savage.Randy               (element, enum, enumBounded, int,
                                             int8, print, word16, word8)
import           Savage.Range               (constantBounded)
import           Silvi.Record               (Field (..), SingField (..),
                                             Value (..), rmap, rtraverse)
import           Silvi.Types
import           System.IO.Unsafe           (unsafePerformIO)
import           Topaz.Rec                  (Rec (..), fromSingList)

rand :: SingField a -> Gen (Value a)
rand = \case
  SingBracketNum  -> ValueBracketNum  <$> randomBracketNum
  SingHttpMethod  -> ValueHttpMethod  <$> randomHttpMethod
  SingHttpStatus  -> ValueHttpStatus  <$> randomHttpStatus
  SingHttpVersion -> ValueHttpVersion <$> randomHttpVersion
  SingUrl         -> ValueUrl         <$> randomUrl
  SingUserId      -> ValueUserId      <$> randomUserident
  SingObjSize     -> ValueObjSize     <$> randomObjSize
  SingIp          -> ValueIp          <$> randomIPv4
  SingTimestamp   -> ValueTimestamp   <$> randomOffsetDatetime

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

randomBracketNum :: Gen BracketNum
randomBracketNum = BracketNum <$> word8 constantBounded

randomHttpMethod :: Gen HttpM.StdMethod
randomHttpMethod = enumBounded

randomHttpStatus :: Gen HttpS.Status
randomHttpStatus = enumBounded

randomHttpVersion :: Gen HttpV.HttpVersion
randomHttpVersion = element [http09, http10, http11, http20]

randomUserident :: Gen UserId
randomUserident = element userIdents

randomObjSize :: Gen ObjSize
randomObjSize = ObjSize <$> word16 constantBounded

randomUrl :: Gen Url
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

randomYear' :: Int -- ^ Origin year
            -> Int -- ^ End year, Usually current year
            -> Gen Year
randomYear' a b = Year <$> enum (min a b) (max a b)

-- | Random year, generated from a given year to the current one.
--
randomYear :: Int -- ^ Origin year
           -> Gen Year
randomYear a = randomYear' a b
  where b = (getYear . dateYear . datetimeDate) $ (timeToDatetime (unsafePerformIO now))

randomMonth' :: Int -- ^ Origin month
             -> Int -- ^ End month, usually current
             -> Gen Month
randomMonth' a b = Month <$> enum (min a b) (max a b)

randomMonth :: Int -- ^ Origin month
            -> Gen Month
randomMonth a = randomMonth' a b
  where b = (getMonth . dateMonth . datetimeDate) $ (timeToDatetime (unsafePerformIO now))

randomDay' :: Int -- ^ Origin Day
           -> Int -- ^ End day, usually the current day
           -> Gen DayOfMonth
randomDay' a b = DayOfMonth <$> enum (min a b) (max a b)

randomDay :: Int -- ^ Origin Day
          -> Gen DayOfMonth
randomDay a = randomDay' a b
  where b = (getDayOfMonth . dateDay . datetimeDate) $ (timeToDatetime (unsafePerformIO now))

randomOffsetDatetime :: Gen OffsetDatetime
randomOffsetDatetime = OffsetDatetime
  <$> randomDatetime
  <*> randomOffset

randomOffset :: Gen Offset
randomOffset = element offsets

-- | List of sample Useridents.
userIdents :: [UserId]
userIdents = map UserId ["-","andrewthad","cement","chessai"]

-- | List of sample URLs.
urls :: [Url]
urls = map Url ["https://github.com","https://youtube.com","layer3com.com"]

-- | List of sample
-- | List of Time Zone Offsets. See:
--   https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
offsets :: [Offset]
offsets = map Offset [100,200,300,330,400,430,500,530,545,600,630,700,800,845,900,930,1000,1030,1100,1200,1245,1300,1345,1400,0,-100,-200,-230,-300,-330,-400,-500,-600,-700,-800,-900,-930,-1000,-1100,-1200]
