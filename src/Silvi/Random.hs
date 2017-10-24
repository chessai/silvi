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
import Savage.Gen ( Gen(..), choose, elements, oneof )

randomHttpMethod :: Gen HttpMethod
randomHttpMethod = elements httpMethods
  
randomHttpStatus :: Gen HttpStatus
randomHttpStatus = elements httpStatuses

randomHttpProtocolVersion :: Gen HttpProtocolVersion
randomHttpProtocolVersion = elements httpProtocolVersions

randomIPv4 :: Gen IPv4
randomIPv4 = fromOctets
  <$> (choose (0,255))
  <*> (choose (0,255))
  <*> (choose (0,255))
  <*> (choose (0,255))

randomDatetime :: Gen Datetime
randomDatetime = Datetime
  <$> randomDate
  <*> randomTimeOfDay

randomTimeOfDay :: Gen TimeOfDay
randomTimeOfDay = TimeOfDay
  <$> (choose (1,24))
  <*> (choose (0,59))
  <*> (choose (0,86399999999999))

randomDate :: Gen Date
randomDate = Date
  <$> (Year <$> choose (1858,2176))
  <*> (Month <$> choose (0,11))
  <*> (DayOfMonth <$> choose (1858,2175))
      

randomOffsetDatetime :: Gen OffsetDatetime
randomOffsetDatetime = OffsetDatetime
  <$> randomDatetime
  <*> randomOffset

randomOffset :: Gen Offset
randomOffset = elements offsets
