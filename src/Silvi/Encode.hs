{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Silvi.Encode
  ( Encode(..)
  ) where

{-# OPTIONS_GHC -Wall #-}

import           Chronos.Types
import           Data.Int
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Word
import qualified Net.IPv4     as I4
import qualified Net.IPv6     as I6
import           Net.Types    (IPv4, IPv6)
import qualified Network.HTTP.Types.Method as HttpM
import qualified Network.HTTP.Types.Status as HttpS
import qualified Network.HTTP.Types.Version as HttpV
import           Silvi.Types

class Encode a where
  encode :: a -> Text
  print  :: a -> IO ()

  default print :: a -> IO ()
  print x = TIO.putStr $ (encode x) `T.append` " "
  default encode :: Show a => a -> Text
  encode = T.pack . show

instance Encode (Value a) where
  encode = \case
    ValueBracketNum  x -> encode x
    ValueHttpMethod  x -> encode x
    ValueHttpStatus  x -> encode x
    ValueHttpVersion x -> encode x 
    ValueUrl         x -> encode x
    ValueUserId      x -> encode x
    ValueObjSize     x -> encode x
    ValueIPv4        x -> encode x
    ValueIPv6        x -> encode x
    ValueTimestamp   x -> encode x
    ValueOffset      x -> encode x
    ValueDatetime    x -> encode x 
    ValueDate        x -> encode x
    ValueYear        x -> encode x
    ValueMonth       x -> encode x
    ValueDayOfMonth  x -> encode x
    ValueTimeOfDay   x -> encode x

instance Encode Text where
  encode = id

instance Encode String where
  encode = T.pack

instance Encode Int where

instance Encode Int8 where

instance Encode Int16 where

instance Encode Int32 where

instance Encode Int64 where

instance Encode Word where

instance Encode Word8 where

instance Encode Word16 where

instance Encode Word32 where

instance Encode Word64 where

instance Encode Url where
  encode (Url x) = x

instance Encode UserId where
  encode (UserId x) = x

instance Encode ObjSize where
  encode (ObjSize x) = encode x

instance Encode BracketNum where
  encode (BracketNum x) = "<" `T.append` encode x `T.append` ">"

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
  encode x = "[" `T.append` encode (offsetDatetimeDatetime x) `T.append` " -" `T.append` encode (offsetDatetimeOffset x) `T.append` "]"

instance Encode Year where
  encode = encode . getYear

instance Encode Month where
  encode = encode . getMonth

instance Encode DayOfMonth where
  encode = encode . getDayOfMonth

instance Encode Date where
  encode x = encode (dateYear x) `T.append` "/" `T.append` encode (dateMonth x) `T.append` "/" `T.append` encode (dateDay x)

instance Encode TimeOfDay where
  encode x = encode (timeOfDayHour x) `T.append` ":" `T.append` encode (timeOfDayMinute x) `T.append` ":" `T.append` encode (timeOfDayNanoseconds x) 

instance Encode Datetime where
  encode x = encode (datetimeDate x) `T.append` encode (datetimeTime x)

instance Encode Offset where
  encode = encode . getOffset
