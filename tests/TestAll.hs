{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Silvi.Random
import Silvi.Types
import Savage
import qualified Data.Text.IO as TIO
import qualified Silvi.Encode as E
import qualified Topaz.Rec as Topaz

type AllTypesLog = '[ 'FieldBracketNum
                    , 'FieldHttpMethod
                    , 'FieldHttpStatus
                    , 'FieldHttpVersion
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

main :: IO ()
main = printLogMany 1000 randAllTypesLog
