{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-missing-signatures #-}

module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Parsers
import           Prelude             hiding (filter)
import           Silvi.LogFormat
import           Silvi.Random
import           Silvi.Record

main :: IO ()
main = run =<< execParser
  (withInfo parseCommands "make them logs with SILVI" "make logs or else")

run :: Command -> IO ()
run (Command n i f t) = putStrLn "hello"


