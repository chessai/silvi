{-# LANGUAGE OverloadedStrings #-}

module Silvi.Builder 
  (
  ) where

import           Data.ByteString.Builder
import           Data.Monoid
import           Silvi.Types

buildHttpMethod :: HttpMethod -> Builder
buildHttpMethod = byteString

buildHttpStatus :: HttpStatus -> Builder
buildHttpStatus (HttpStatus i b) = 
     (intDec i) 
  <> (byteString b)

buildHttpProtocol :: HttpProtocol -> Builder
buildHttpProtocol = stringUtf8 . show

buildHttpProtocolVersion :: HttpProtocolVersion -> Builder
buildHttpProtocolVersion (HttpProtocolVersion majour minour) =
     (intDec majour)
  <> (intDec minour)

--buildUrl :: Url -> Builder
--buildUrl (Url u) = 

--builderUserId :: UserId -> Builder

--buildObjSize :: ObjSize -> Builder




