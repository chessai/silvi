{-# LANGUAGE OverloadedStrings #-}

module Silvi.Random 
  ( randomIPv4
  , randomDatetime
  , randomOffsetDatetime
  , randomHttpStatus
  , randomHttpProtocol
  ) where

import Silvi.Types

import Chronos.Types
import Data.Word (Word8)
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Savage.Gen ( Gen(..), choose, oneof )

randomHttpStatus :: Gen HttpStatus
randomHttpStatus = do
  hs <- oneof $ fmap pure [status200,status204,status301,status400,status401,status403,status404,status405,status500,status503,status504]
  pure hs

randomHttpProtocol :: Gen HttpVersion
randomHttpProtocol = do
  hp <- oneof $ fmap pure [http09,http10,http11,http20]
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
  o <- oneof $ fmap pure [100,200,300,330,400,430,500,530,545,600,630,700,800,845,900,930,1000,1030,1100,1200,1245,1300,1345,1400,0,(-100),(-200),(-230),(-300),(-330),(-400),(-500),(-600),(-700),(-800),(-900),(-930),(-1000),(-1100),(-1200)]
  pure (Offset o)
