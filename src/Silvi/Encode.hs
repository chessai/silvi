{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Silvi.Encode
  ( Encode(..)
  ) where

{-# OPTIONS_GHC -Wall #-}

import           Chronos.Types
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Functor.Identity (runIdentity)
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Net.IPv4     as I4
import qualified Net.IPv6     as I6
import           Net.Types    (IPv4, IPv6)
import qualified Network.HTTP.Types.Method as HttpM
import qualified Network.HTTP.Types.Status as HttpS
import qualified Network.HTTP.Types.Version as HttpV
import           Savage
import qualified Savage.Internal.Gen as Gen 
import           Savage.Internal.Seed (Seed)
import           Savage.Internal.Seed as Seed
import           Savage.Internal.Tree (Tree(..), Node(..))
import qualified Savage.Internal.Tree as Tree
import           Savage.Range (Size, Range)
import qualified Savage.Range as Range
import           Silvi.Types

class Encode a where
  {-# MINIMAL encode #-} 
  encode :: a -> Text
  print  :: a -> IO ()
  default print :: a -> IO ()
  print = TIO.putStrLn . encode

instance (Show a, Num a) => Encode a where
  encode = T.pack . show

instance Encode Url where
  encode (Url x) = x

instance Encode UserId where
  encode (UserId x) = x

instance Encode ObjSize where
  encode (ObjSize x) = T.pack $ show x

instance Encode BracketNum where
  encode (BracketNum x) = T.pack $ "<" ++ show x ++ ">"

instance Encode IPv4 where
  encode = I4.encode
  print  = I4.print

instance Encode IPv6 where
  encode = I6.encode
  print  = I6.print

instance Encode HttpM.StdMethod where
  encode = T.pack . show

instance Encode HttpS.Status where
  encode = T.pack . show . HttpS.statusCode

instance Encode HttpV.HttpVersion where
  encode = T.pack . show

instance Encode OffsetDatetime where
  encode x = undefined
  --T.pack $ "[" ++ show x ++ " -" ++ show x ++ "]" 

instance Encode Year where
  encode = T.pack . show . getYear

instance Encode Month where
  encode = T.pack . show . getMonth

instance Encode DayOfMonth where
  encode = T.pack . show . getDayOfMonth

instance Encode Date where
  encode x = encode (dateYear x) T.++ "/" T.++ encode (dateMonth x) T.++ "/" T.++ encode (dateDay x)

instance Encode TimeOfDay where
  encode x = encode (timeOfDayHour x) T.++ ":" T.++ encode (timeOfDayMinute x) T.++ ":" T.++ encode (timeOfDay x) 


