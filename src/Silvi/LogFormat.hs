{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Silvi.LogFormat
  ( TestLog
  ) where

import           Silvi.Record (Field (..), SingField (..), Value (..))
import           Silvi.Types

{-
 - Palo Alto logs formats, as described in:
 - https://www.paloaltonetworks.com/documentation/61/pan-os/pan-os/reports-and-logging/syslog-field-descriptions.html
 -
-}

type TestLog = '[ FieldBracketNum
                , FieldIp
                ]

type PaloAltoTraffic = '[ ]

type PaloAltoThreat = '[]

type PaloAltoHIPMatch = '[]

type PaloAltoConfig = '[]

type PaloAltoSystem = '[]

type PaloAltoSeverity = '[]

--type PaloAltoEvent = []

