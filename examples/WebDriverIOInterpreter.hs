{-# language UndecidableInstances #-}

module WebDriverIOInterpreter where

import Data.Aeson
import Data.Text.IO qualified as T
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  liftIO,
  runEff,
  type (:>),
 )
import Effectful.Reader.Dynamic
import Effectful.Dispatch.Dynamic (
  interpret, send, localSeqUnliftIO, localSeqUnlift
 )
import Web.Api.WebDriver ( WebDriverT )
import WebDriverEffect ( WebUI(..) )
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client qualified as L
import Network.HTTP.Req as R
    ( Url,
      Scheme(Http),
      HttpMethod(AllowsBody),
      GET(GET),
      NoReqBody(NoReqBody),
      HttpBody,
      toVanillaResponse,
      ProvidesBody,
      HttpBodyAllowed,
      http,
      (/:),
      runReq,
      defaultHttpConfig,
      req,
      jsonResponse,
      port,
      responseStatusCode,
      responseStatusMessage,
      responseBody,
      responseCookieJar )
import Network.HTTP.Types qualified as L
import PyrethrumExtras (getLenient, toS, txt, uu)
import Prelude hiding (get)


type MyWebDriver eff a = WebDriverT (Eff eff) a


-- instance (WebUI :> es) => WebDriverT (Eff es) a where
--     randomInt = send RandomInt


runWebDriverIO' :: forall es a. ( IOE :> es) => Eff (WebUI : es) a  -> Eff es a
runWebDriverIO' =
  interpret $ \_ ->
    EF.liftIO . \case
      Click css -> uu
      Go url -> uu -- navigateTo url
      Sleep i -> uu -- wait i
      Read css -> uu -- findElement CssSelector css >>= getElementText) 

-- $> driverRunning
driverRunning :: IO Bool
driverRunning = (==) 200 . (.statusCode) <$> get1 "status"

gheckoUrl :: R.Url 'Http
gheckoUrl = http "127.0.0.1" /: "status"

logging :: Bool
logging = True


driver :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body),  Foldable t, MonadIO m, HttpMethod method, HttpBody body) => method -> body -> t Text -> m HttpResponse
driver = driver' logging

get :: [Text] -> IO HttpResponse
get = driver GET NoReqBody

get1 :: Text -> IO HttpResponse
get1 = get . pure

get2 :: Text -> Text -> IO HttpResponse
get2 s1 s2 = get [s1, s2]

driver' :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body),  Foldable t, MonadIO m, HttpMethod method, HttpBody body) => Bool -> method -> body -> t Text -> m HttpResponse
driver' log method body subDirs = runReq defaultHttpConfig $ do
  r <- req method url body jsonResponse $ port 4444
  let rslt =
        MkHttpResponse
          { statusCode = responseStatusCode r,
            statusMessage = getLenient . toS $ responseStatusMessage r,
            headers = L.responseHeaders . toVanillaResponse $ r,
            body = responseBody r :: Value,
            cookies = responseCookieJar r
          }
  liftIO $ do
    when log $ 
      T.putStrLn . txt $ rslt
    pure $
      MkHttpResponse
        { statusCode = responseStatusCode r,
          statusMessage = getLenient . toS $ responseStatusMessage r,
          headers = L.responseHeaders . toVanillaResponse $ r,
          body = responseBody r :: Value,
          cookies = responseCookieJar r
        }
  where
    url :: R.Url 'Http
    url = foldl' (/:) (http "127.0.0.1") subDirs


data HttpResponse = MkHttpResponse
  { statusCode :: Int,
    statusMessage :: Text,
    headers :: L.ResponseHeaders,
    body :: Value,
    cookies :: L.CookieJar
  }
  deriving (Show)

data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile label action = send (Profile label action)

runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
   Profile label action -> localSeqUnliftIO env $ \unlift -> do
     t1 <- getMonotonicTime
     r <- unlift action
     t2 <- getMonotonicTime
     putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
     pure r

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
   Profile _label action -> localSeqUnlift env $ \unlift -> unlift action
   
action' :: forall es. (Profiling :> es, IOE :> es) => Eff es ()
action' = profile "greet" . liftIO $ putStrLn "Hello!"

-- $> profileAction
profileAction :: IO ()
profileAction = runEff . runProfiling $ action'
