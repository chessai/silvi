{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Semigroup ( (<>) )
import Data.Text (Text)
import Options.Applicative
import Silvi.LogFormat
import Silvi.Random
import Silvi.Record

type Number   = Int  -- ^ Number of logs to generate.                              Default = 1,000
type Include  = Text -- ^ List of log types to generate, e.g. "[PaloAlto, Aruba]"  Default = [<all>]
type FilterBy = Text -- ^ Generate all log types, quotiented by a list of logs.    Default = []
type Time     = Text -- ^ The time from which to generate the logs, to now.
                     --   This will generate logs increasing in time, with the 
                     --   acting interval behing [time given, currrentPosixTime]

withInfo :: Parser a -> Text -> Text -> ParserInfo a
withInfo opts desc head = info (opts <**> helper)
    (fullDesc
  <> progDesc desc
  <> header head
    )

number :: Parser Number
number = option auto
    (long "number"
  <> short 'n'
  <> help "Number of logs to generate."
  <> showDefault
  <> value 1000
  <> metavar "INT"
    )

include :: Parser Include
include = strOption 
        (long "include"
      <> short 'i'
      <> help "List of log types to generate, e.g. '<PaloAlto,Aruba>'"
      <> showDefault
      <> value "[all]"
      <> metavar "LIST"
        )

filterBy :: Parser FilterBy
filterBy = strOption 
        (long "filter"
      <> short 'f'
      <> help "Generate all log types, quotiented by a list of logs."
      <> showDefault
      <> value "[] (empty list)"
      <> metavar "LIST"
        )
main :: IO ()
main = do
  options <- execParser opts
  pure ()

