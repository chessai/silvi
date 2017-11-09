{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Silvi.LogFormat 
  (
  ) where

import Silvi.Types
import Silvi.Record ( Field(..), Value(..), SingField(..) )

{-
 - Palo Alto logs formats, as described in:
 - https://www.paloaltonetworks.com/documentation/61/pan-os/pan-os/reports-and-logging/syslog-field-descriptions.html
 -
-}

type PaloAltoTraffic = '[]

type PaloAltoThreat = '[]

type PaloAltoHIPMatch = '[]

type PaloAltoConfig = '[]

type PaloAltoSystem = '[]

type PaloAltoSeverity = '[]

--type PaloAltoEvent = []

