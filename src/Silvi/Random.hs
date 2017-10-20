{-# LANGUAGE OverloadedStrings #-}

module Silvi.Random 
  (
  ) where

import Silvi.Types

import Chronos.Types
import Savage.Gen

randomDatetime :: Gen Datetime
randomDatetime = do
  d <- randomDate
  tod <- randomTimeOfDay
  pure (Datetime d tod)

randomTimeOfDay :: Gen TimeOfDay
randomTimeOfDay = do
  hour <- choose (1,24)
  minute <- choose (0,59)
  nanoseconds <- choose (0, 2^63)
  pure (TimeOfDay hour minute nanoseconds)

randomDate :: Gen Date
randomDate = do
  d <- choose (1,31)
  m <- choose (0,11)
  y <- choose (1858,2176)
  pure (Date (Year (y)) (Month (m)) (DayOfMonth (d)))

--randomOffset :: Gen Offset
--randomOffsetDatetime :: Gen OffsetDatetime
  
