-- -*- coding: utf-8; mode: haskell; -*-

-- File: src/Silvi/Tutorial.hs
--
-- License:
--     Copyright 2017 Daniel Cartwright
--
--     Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
--     1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
--     2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
--     3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |
--   Module     : Silvi.Tutorial
--   Copyright  : Copyright 2017 Daniel Cartwright
--   License    : BSD3
--   Maintainer : dcartwright@layer3com.com
--   Stability  : stable
module Silvi.Tutorial
  ( -- * Introduction
    -- $introduction

    -- * Defining Log Formats
    -- $def_logs
     
    -- * Generating Random Logs
    -- $rand_logs
    --
    --
    randSimpleLog
  , main
  ) where

import           Silvi.Random (randLog, printMany, Gen)
import           Silvi.Record (Field (..), Value (..), Rec(..))

--------------------------------------------------------------------------------

-- $introduction
--
-- This library allows you to generate random logs, as well as easily construct
-- any sort of log you might yourself want.
--
-- In general, when using @silvi@, you'll want the following language extensions:
--
-- @
-- {-# LANGUAGE DataKinds         #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications  #-}
-- @
--
-- And the following imports:
--
-- @
-- import Silvi.Random
-- import Silvi.Record
-- @
-- 

---------------------------------------------------------------------------------

-- $def_logs
--
-- > -- It is easy to define a log. 
-- > -- A log is represented as a type-level list consisting of different fields.
-- >
-- > -- 'SimpleLog' represents a Log consisting of just an IPv4 address and a URL.
-- > type SimpleLog = '[ FieldIp
-- >                   , FieldUrl
-- >                   ]

type SimpleLog = '[ 'FieldIPv4
                  , 'FieldUrl
                  ]

type AdvancedLog = '[ 'FieldBracketNum
                    , 'FieldTimestamp
                    , 'FieldIPv4
                    , 'FieldUrl
                    ] 

---------------------------------------------------------------------------------

-- $rand_logs
-- >
-- > -- 'randSimpleLog' now contains a SimpleLog with randomised inhabitants of the IPv4 and URL types.
-- > randSimpleLog :: Gen (Rec Value as)
-- > randSimpleLog = randLog @SimpleLog
-- > 
-- > -- 'main' will print out 1000 'SimpleLog's.
-- > main :: IO ()
-- > main = printMany 1000 randSimpleLog

randSimpleLog :: Gen (Rec Value SimpleLog)
randSimpleLog = randLog @SimpleLog

randAdvancedLog :: Gen (Rec Value AdvancedLog)
randAdvancedLog = randLog @AdvancedLog

main :: IO ()
main = printMany 10 randAdvancedLog
