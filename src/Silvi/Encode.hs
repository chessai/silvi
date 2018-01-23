{-# LANGUAGE DefaultSignatures #-}

module Silvi.Encode
  ( Encode(..)
  ) where

{-# OPTIONS_GHC -Wall #-}

import           Silvi.Types
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Net.IPv4     as I4
import qualified Net.IPv6     as I6
import           Net.Types    (IPv4, IPv6)

class Encode a where
  {-# MINIMAL encode #-} 
  encode :: a -> Text
  print  :: a -> IO ()
  default print :: a -> IO ()
  print = TIO.putStrLn . encode

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

