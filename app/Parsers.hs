{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-missing-signatures #-}

module Parsers where

import Data.Semigroup ( (<>) )
import Options.Applicative
import Prelude hiding (filter)

data Command = 
  Command { number  :: Int
          , include :: String
          , filter  :: String
          , time    :: String
          }

parseNumber :: Parser Int
parseNumber = option auto
    (long "number"
  <> short 'n'
  <> help "Number of logs to generate."
  <> showDefault
  <> value 1000
    )

parseInclude :: Parser String
parseInclude = strOption
    (long "include"
  <> short 'i'
  <> help "List the log types to generate, e.g. '[PaloAlto,Aruba]'"
  <> showDefault
  <> value "[all logs]"
    )

parseFilter :: Parser String
parseFilter = strOption
    (long "filter"
  <> short 'f'
  <> help "Generate all types of logs, quotiented by a provided list of logs."
  <> showDefault
  <> value "[] (empty list)"
    )

parseTime :: Parser String
parseTime = strOption
    (long "time"
  <> short 't'
  <> help ("The time from which to generate the logs, to now.\n"
       ++ "This will generate logs that increase in time with a random step.\n"
       ++ "Example: --time 1y 2mo 3d 4h 5min 6s will generate a number of logs \n"
       ++ "from 1 year, 2 months, 3 days, 4 hours, 5 minutes, and 6 seconds \n"
       ++ "in the past, to now. \n" 
       ++ "Note that these numbers are bounded in the following way: \n" 
       ++ "[0,currentYear - 1971]y [0,11]mo [0,31]d, [0,23]h, [0,59]m, [0,59]s \n"
       ++ "Any omitted time is defaulted to 0.")
  <> showDefault
  <> value "12h"
    )


withInfo :: Parser a -> String -> String -> ParserInfo a
withInfo p d h = info (p <**> helper)
    (fullDesc
  <> progDesc d
  <> header h
    )

parseCommands :: Parser Command
parseCommands = Command
  <$> parseNumber
  <*> parseInclude
  <*> parseFilter
  <*> parseTime
