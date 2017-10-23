{-# LANGUAGE OverloadedStrings #-}

module Silvi.Random 
  ( randomIPv4
  , randomOffsetDatetime
  , randomHttpMethod
  , randomHttpStatus
  , randomHttpProtocolVersion
  ) where

import Silvi.Types

import Chronos.Types
import Data.Word (Word8)
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Savage.Gen ( Gen(..), choose, oneof )

randomHttpMethod :: Gen HttpMethod
randomHttpMethod = do
  hm <- oneof $ fmap pure httpMethods
  pure hm
  
randomHttpStatus :: Gen HttpStatus
randomHttpStatus = do
  hs <- oneof $ fmap pure httpStatuses
  pure hs

randomHttpProtocolVersion :: Gen HttpProtocolVersion
randomHttpProtocolVersion = do
  hp <- oneof $ fmap pure httpProtocolVersions
  pure hp

randomIPv4 :: Gen IPv4
randomIPv4 = do
  d1 <- choose (0,255)
  d2 <- choose (0,255)
  d3 <- choose (0,255)
  d4 <- choose (0,255)
  pure (fromOctets d1 d2 d3 d4)

randomDatetime :: Gen Datetime
randomDatetime = do
  d <- randomDate
  tod <- randomTimeOfDay
  pure (Datetime d tod)

randomTimeOfDay :: Gen TimeOfDay
randomTimeOfDay = do
  hour <- choose (1,24)
  minute <- choose (0,59)
  nanoseconds <- choose (0, 86399999999999)
  pure (TimeOfDay hour minute nanoseconds)

randomDate :: Gen Date
randomDate = do
  d <- choose (1,31)
  m <- choose (0,11)
  y <- choose (1858,2176)
  pure (Date (Year (y)) (Month (m)) (DayOfMonth (d)))

randomOffsetDatetime :: Gen OffsetDatetime
randomOffsetDatetime = do
  dt <- randomDatetime
  o  <- randomOffset
  pure (OffsetDatetime dt o)

-- https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
randomOffset :: Gen Offset
randomOffset = do
  o <- oneof $ fmap pure offsets
  pure (Offset o)
