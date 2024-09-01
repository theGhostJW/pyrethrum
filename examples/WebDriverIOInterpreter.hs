module WebDriverIOInterpreter where

import Data.Aeson
import Data.Text.IO qualified as T
import Effectful as EF
  ( Eff,
    IOE,
    liftIO,
    type (:>),
  )
-- import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic
  ( interpret,
  )
import Network.HTTP.Client qualified as L
import Network.HTTP.Req as R
  ( DELETE (DELETE),
    GET (GET),
    HttpBody,
    HttpBodyAllowed,
    HttpException,
    HttpMethod (AllowsBody),
    NoReqBody (NoReqBody),
    POST (POST),
    ProvidesBody,
    ReqBodyJson (ReqBodyJson),
    Scheme (Http),
    Url,
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    responseBody,
    responseCookieJar,
    responseStatusCode,
    responseStatusMessage,
    runReq,
    toVanillaResponse,
    (/:),
  )
import Network.HTTP.Types qualified as L
import PyrethrumExtras (delete, getLenient, toS, txt, uu)
import UnliftIO (try)
import Web.Api.WebDriver (Capabilities, WebDriverT, defaultFirefoxCapabilities)
import WebDriverEffect (WebUI (..), SessionId(..))
import Prelude hiding (get)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.KeyMap (lookup, singleton)
import Data.Aeson.Types (parseMaybe)
import WebDriverPure
import UnliftIO.Concurrent (threadDelay)
import WebDriverRawIO

type MyWebDriver eff a = WebDriverT (Eff eff) a

runWebDriverIO' :: forall es a. ( IOE :> es) => Eff (WebUI : es) a  -> Eff es a
runWebDriverIO' =
  interpret $ \_ ->
    EF.liftIO . \case
      NewSession -> newFirefoxSession
      KillSession sessionId -> deleteSession_ sessionId
      Click sessionId css -> uu
      Go sessionId url -> navigateTo_ sessionId url 
      Sleep milliSec -> threadDelay $ 1_000 * milliSec
      Read sessionId css -> uu -- findElement CssSelector css >>= getElementText)



