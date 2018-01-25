{-# LANGUAGE GADTs #-}

module Silvi.Types
  ( Url(..)
  , UserId(..)
  , ObjSize(..)
  , BracketNum(..)
  , Silvi 
  ) where

{-# OPTIONS_GHC -Wall #-}

import           Data.Exists
import           Data.Text (Text)
import           Topaz.Rec (Rec(..))
import           Savage
import           Silvi.Internal.Types
import           Silvi.Record (Field(..), Value(..), SingField(..), Rec(..))

type Silvi a = Gen (Rec Value a)
