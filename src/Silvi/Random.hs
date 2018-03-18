{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Silvi.Random
  ( randLogExplicit
  , randLog
  , printLog 
  , printLogMany 
  ) where

import qualified Chronos
import           Chronos.Types
import           Control.Applicative (liftA2)
import           Control.Monad       (join, replicateM_)
import           Data.Exists         (Reify (..), SingList (..))
import Data.Kind (Type)
import qualified Data.Text.IO        as TIO
import           Net.IPv4            (ipv4)
import           Net.IPv6            (ipv6)
import           Net.Types           (IPv4(..),IPv6(..))
import           Savage
import           Savage.Randy        (sample, element, enum, enumBounded, word8, word16)
import           Savage.Range        (constantBounded)
import Silvi.Cover
import qualified Silvi.Encode        as E
--import           Silvi.Types
import Silvi.Types hiding (Silvi)
import           Topaz.Rec           (Rec (..), fromSingList)
import qualified Topaz.Rec           as Topaz
import qualified Network.HTTP.Types.Method  as HttpM
import qualified Network.HTTP.Types.Status  as HttpS
import           Network.HTTP.Types.Version (http09, http10, http11, http20)
import qualified Network.HTTP.Types.Version as HttpV

--type Silvi v a = Gen (Rec v a)

type Silvi'
  (v :: k -> Type)
  (a :: [k])
  = Gen (Rec v a)

type Silvi''
  (v :: k -> Type)
  (a :: k)
  = Gen (v a)

type Silvi   as = Silvi'  Value as
type UnSilvi as = Silvi'' Value as

--rand :: Generic f => 

--rand' :: 
--rand' :: k
--rand :: X -> Gen Y
--randOne :: forall as. (Reify as) => Silvi as
--randOneExplicit :: Rec SingField rs -> Silvi rs

rand :: SingField a -> UnSilvi a
rand = \case
  SingBracketNum   -> ValueBracketNum  <$> randomBracketNum
  SingHttpMethod   -> ValueHttpMethod  <$> randomHttpMethod
  SingHttpStatus   -> ValueHttpStatus  <$> randomHttpStatus
  SingHttpVersion  -> ValueHttpVersion <$> randomHttpVersion
  SingHttpProtocol -> ValueHttpProtocol<$> randomHttpProtocol
  SingUrl          -> ValueUrl         <$> randomUrl
  SingUserId       -> ValueUserId      <$> randomUserident
  SingObjSize      -> ValueObjSize     <$> randomObjSize
  SingIPv4         -> ValueIPv4        <$> randomIPv4
  SingIPv6         -> ValueIPv6        <$> randomIPv6 
  SingTimestamp    -> ValueTimestamp   <$> randomOffsetDatetime
  SingOffset       -> ValueOffset      <$> randomOffset
  SingDatetime     -> ValueDatetime    <$> randomDatetime
  SingDate         -> ValueDate        <$> randomDate
  SingYear         -> ValueYear        <$> randomYear 1995 2021
  SingMonth        -> ValueMonth       <$> randomMonth
  SingDayOfMonth   -> ValueDayOfMonth  <$> (join $ liftA2 randomDayOfMonth (randomYear 1995 2021) (randomMonth))
  SingTimeOfDay    -> ValueTimeOfDay   <$> randomTimeOfDay

randLog :: forall as. (Reify as) => Silvi as
randLog = randLogExplicit (fromSingList (reify :: SingList as))

randLogExplicit :: Rec SingField rs -> Silvi rs
randLogExplicit = Topaz.traverse rand

printLog :: Silvi as -> IO ()
printLog = join . sample . fmap (Topaz.traverse_ E.print)

printLogMany :: Int -> Silvi as -> IO ()
printLogMany n gen = replicateM_ n (printLog gen >> TIO.putStrLn "")

randomIPv4 :: Gen IPv4
randomIPv4 = ipv4
  <$> word8 constantBounded
  <*> word8 constantBounded
  <*> word8 constantBounded
  <*> word8 constantBounded

randomIPv6 :: Gen IPv6
randomIPv6 = ipv6
  <$> word16 constantBounded
  <*> word16 constantBounded
  <*> word16 constantBounded
  <*> word16 constantBounded
  <*> word16 constantBounded
  <*> word16 constantBounded
  <*> word16 constantBounded
  <*> word16 constantBounded

randomBracketNum :: Gen BracketNum
randomBracketNum = BracketNum <$> word8 constantBounded

randomHttpMethod :: Gen HttpM.StdMethod
randomHttpMethod = enumBounded

randomHttpStatus :: Gen HttpS.Status
randomHttpStatus = enumBounded

randomHttpVersion :: Gen HttpV.HttpVersion
randomHttpVersion = element [http09, http10, http11, http20]

randomHttpProtocol :: Gen HttpProtocol
randomHttpProtocol = element [HTTPS, HTTP, FTP]

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
randomDate = undefined
--do
--  let year  = randomYear 1995 2021
--      month = enumBounded
--      day   = liftA2 randomDayOfMonth year month
--  Date <$> year <*> month <*> (join day)

randomYear :: Int -- ^ Origin year
           -> Int -- ^ End year
           -> Gen Year
randomYear a b = Year <$> enum (min a b) (max a b)

randomMonth :: Gen Month
randomMonth = undefined
--enumBounded

randomDayOfMonth :: Year
                 -> Month
                 -> Gen DayOfMonth
randomDayOfMonth year month = DayOfMonth <$> enum 1 b
  where b = (\y m -> Chronos.daysInMonth (Chronos.isLeapYear y) m) year month

randomOffsetDatetime :: Gen OffsetDatetime
randomOffsetDatetime = OffsetDatetime
  <$> randomDatetime
  <*> randomOffset

randomOffset :: Gen Offset
randomOffset = element offsets

-- | List of sample
-- | List of Time Zone Offsets. See:
--   https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
offsets :: [Offset]
offsets = fmap Offset [100,200,300,330,400,430,500,530,545,600,630,700,800,845,900,930,1000,1030,1100,1200,1245,1300,1345,1400,0,-100,-200,-230,-300,-330,-400,-500,-600,-700,-800,-900,-930,-1000,-1100,-1200]

-- | List of sample Useridents.
userIdents :: [UserId]
userIdents = fmap UserId ["-","andrewthad","cement","chessai"]

-- | List of sample URLs.
urls :: [Url]
urls = fmap Url ["https://github.com","https://youtube.com","layer3com.com"]
