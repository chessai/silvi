{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Text (Text)
import Silvi.Random
import Silvi.Types
import Savage
import Savage.Randy (sample)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Silvi.Encode as E
import qualified Topaz.Rec as Topaz

testAnnounce :: Text -> IO ()
testAnnounce x = do
  TIO.putStrLn "================================"
  replicateM_ ((32 - T.length x) `div` 2) (TIO.putStr "=") 
  TIO.putStr x
  replicateM_ ((32 - T.length x) `div` 2) (TIO.putStr "=") 
  TIO.putStrLn "" 
  TIO.putStrLn "================================"

success :: Text -> IO ()
success x = do
  TIO.putStrLn ""
  TIO.putStrLn $ "Succeeded: " `T.append` x

ms :: Silvi a -> Text -> IO ()
ms s name = do
  testAnnounce name
  let ot :: Gen (IO ())
      ot = fmap (Topaz.traverse_ E.print) s
      ok :: IO (IO ())
      ok = sample ot
  join ok
  success name

main :: IO ()
main = do
  ms (randLog @('[ 'FieldBracketNum ])) "BracketNum"
  ms (randLog @('[ 'FieldHttpMethod ])) "HttpMethod"
  ms (randLog @('[ 'FieldHttpStatus ])) "HttpStatus"
  ms (randLog @('[ 'FieldHttpVersion])) "HttpVersion"
  ms (randLog @('[ 'FieldHttpProtocol])) "HttpProtocol"
  ms (randLog @('[ 'FieldUrl ])) "Url"
  ms (randLog @('[ 'FieldUserId])) "UserId"
  ms (randLog @('[ 'FieldObjSize ])) "ObjSize"
  ms (randLog @('[ 'FieldIPv4 ])) "IPv4"
  ms (randLog @('[ 'FieldIPv6 ])) "IPv6"
  ms (randLog @('[ 'FieldTimestamp ])) "OffsetDatetime"
  ms (randLog @('[ 'FieldOffset ])) "Offset"
  ms (randLog @('[ 'FieldDatetime ])) "Datetime"
  ms (randLog @('[ 'FieldDate ])) "Date"
  ms (randLog @('[ 'FieldYear ])) "Year"
  ms (randLog @('[ 'FieldMonth ])) "Month"
  ms (randLog @('[ 'FieldDayOfMonth ])) "DayOfMonth"
  ms (randLog @('[ 'FieldTimeOfDay ])) "TimeOfDay"
  
  printMany 100 randAllTypesLog

type AllTypesLog = '[ 'FieldBracketNum
                    , 'FieldHttpMethod
                    , 'FieldHttpStatus
                    , 'FieldHttpVersion
                    , 'FieldHttpProtocol 
                    , 'FieldUrl
                    , 'FieldUserId
                    , 'FieldObjSize
                    , 'FieldIPv4
                    , 'FieldIPv6
                    , 'FieldTimestamp
                    , 'FieldOffset
                    , 'FieldDatetime
                    , 'FieldDate
                    , 'FieldYear
                    , 'FieldMonth
                    , 'FieldDayOfMonth
                    , 'FieldTimeOfDay
                    ]

randAllTypesLog :: Silvi AllTypesLog
randAllTypesLog = randLog @AllTypesLog
