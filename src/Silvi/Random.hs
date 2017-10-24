{-# LANGUAGE OverloadedStrings #-}

module Silvi.Random 
  ( randomLogEntry
  ) where

import Silvi.Types

import Chronos.Types
import Data.Text (Text)
import Data.Word (Word8)
import Net.IPv4 (fromOctets)
import Net.Types (IPv4)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Savage.Gen ( Gen(..), choose, elements, oneof )

randomLogEntry :: Gen LogEntry
randomLogEntry = LogEntry
  <$> randomIPv4
  <*> randomUserident
  <*> randomUserident
  <*> randomOffsetDatetime
  <*> randomHttpMethod
  <*> randomUrl
  <*> randomHttpProtocol
  <*> randomHttpProtocolVersion
  <*> randomHttpStatus
  <*> randomObjSize

randomIPv4 :: Gen IPv4
randomIPv4 = fromOctets
  <$> (choose (0,255))
  <*> (choose (0,255))
  <*> (choose (0,255))
  <*> (choose (0,255))

randomHttpMethod :: Gen HttpMethod
randomHttpMethod = elements httpMethods
  
randomHttpStatus :: Gen HttpStatus
randomHttpStatus = elements httpStatuses

randomHttpProtocol :: Gen HttpProtocol
randomHttpProtocol = elements httpProtocols

randomHttpProtocolVersion :: Gen HttpProtocolVersion
randomHttpProtocolVersion = elements httpProtocolVersions

randomUserident :: Gen Text
randomUserident = elements userIdents

randomObjSize :: Gen Int
randomObjSize = choose (8,10000)

randomQuote :: Gen Text
randomQuote = elements quotes

randomUrl :: Gen Text
randomUrl = elements urls

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

-- | List of HTTP Methods.
httpMethods :: [HttpMethod]
httpMethods = [methodGet,methodPost,methodHead,methodPut,methodDelete,methodTrace    ,methodConnect,methodOptions,methodPatch]

-- | List of HTTP Protocols.
httpProtocols :: [HttpProtocol]
httpProtocols = [HTTP,HTTPS,FTP]

-- | List of HTTP Protocol Versions.
httpProtocolVersions :: [HttpProtocolVersion]
httpProtocolVersions = [http09,http10,http11,http20]

-- | List of HTTP Status Codes.
httpStatuses :: [HttpStatus]
httpStatuses = [status200,status204,status301,status400,status401,status403,status404,status405,status500,status503,status504]

-- | List of sample Useridents.
userIdents :: [Text]
userIdents = ["-","userFoo","userBar","userBaz"]

-- | List of sample URLs.
urls :: [Text]
urls = ["https://github.com","http://bar.com","ftp://baz.com"]

quotes :: [Text]
quotes = ["customerA got caught putting their hand in the cookie jar","customerB uses firefox instead of chrome"]

-- | List of sample
-- | List of Time Zone Offsets. See:
--   https://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
offsets :: [Offset]
offsets = map Offset [100,200,300,330,400,430,500,530,545,600,630,700,800,845,900,930,1000,1030,1100,1200,1245,1300,1345,1400,0,(-100),(-200),(-230),(-300),(-330),(-400),(-500),(-600),(-700),(-800),(-900),(-930),(-1000),(-1100),(-1200)]
