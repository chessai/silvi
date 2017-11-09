module Silvi.Types
  ( HttpMethod(..)
  , HttpStatus(..)
  , HttpProtocol(..)
  , HttpProtocolVersion(..)
  , Url(..)
  , UserId(..)
  , ObjSize(..)
  , IPv4(..)
  , OffsetDatetime(..)
  ) where

import           Chronos.Types              (Offset (..), OffsetDatetime (..))
import           Data.Text                  (Text)
import           Net.Types                  (IPv4 (..))
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Version

-- | Type alias for Netowrk.HTTP.Types.Method, renamed
--   to be more explicit.
--
type HttpMethod = Method

-- | Type alias for Network.HTTP.Types.Status, renamed
--   to be more explicit.
--
type HttpStatus = Status

-- | The HTTP Protocol used.
--
data HttpProtocol = HTTP | HTTPS | FTP deriving (Show, Eq)

-- | Type alias for Network.HTTP.Types.Version, renamed
--   to be more explicit.
--
type HttpProtocolVersion = HttpVersion

-- | Url type.
--   TODO: Expand on this for better randomisation.
--
newtype Url = Url { getUrl :: Text }
  deriving (Show,Eq)

-- | UserId type.
--
newtype UserId = UserId { getUserId :: Text }
  deriving (Show,Eq)

-- | Requested resource size.
--
newtype ObjSize = ObjSize { getObjSize :: Text }
  deriving (Show,Eq)
