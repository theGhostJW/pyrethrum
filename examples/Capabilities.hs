{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Capabilities where

import Data.Aeson.Key (fromText)
import Data.Aeson.Types
    ( (.:),
      (.:?),
      withObject,
      withText,
      object,
      FromJSON(parseJSON),
      Object,
      Value,
      KeyValue((.=)),
      ToJSON(toJSON),
      Pair,
      Parser )
import WebDriverPure (opt)
import Prelude hiding (Proxy)

-- Custom Types for Enums
data UnhandledPromptBehavior
  = Dismiss
  | Accept
  | DismissAndNotify
  | AcceptAndNotify
  | Ignore
  deriving (Show, Generic)

data PageLoadStrategy
  = None
  | Eager
  | Normal
  deriving (Show, Generic)

data BrowserName
  = Chrome
  | Firefox
  | Safari
  | Edge
  | InternetExplorer
  deriving (Show, Generic)

data PlatformName
  = Windows
  | Mac
  | Linux
  | Android
  | IOS
  deriving (Show, Generic)

-- Core Capabilities
data Capabilities = Capabilities
  { browserName :: BrowserName,
    browserVersion :: Maybe Text,
    platformName :: Maybe PlatformName,
    acceptInsecureCerts :: Maybe Bool,
    pageLoadStrategy :: Maybe PageLoadStrategy,
    proxy :: Maybe Proxy,
    timeouts :: Maybe Timeouts,
    strictFileInteractability :: Maybe Bool,
    unhandledPromptBehavior :: Maybe UnhandledPromptBehavior,
    vendorSpecific :: Maybe VendorSpecific
  }
  deriving (Show, Generic)

-- Proxy Configuration
data Proxy = Proxy
  { proxyType :: Text,
    proxyAutoconfigUrl :: Maybe Text,
    ftpProxy :: Maybe Text,
    httpProxy :: Maybe Text,
    sslProxy :: Maybe Text,
    noProxy :: Maybe [Text]
  }
  deriving (Show, Generic)

-- Timeouts Configuration
data Timeouts = Timeouts
  { implicit :: Maybe Int,
    pageLoad :: Maybe Int,
    script :: Maybe Int
  }
  deriving (Show, Generic)

-- Vendor-Specific Capabilities
data VendorSpecific
  = ChromeOptions
      { chromeArgs :: Maybe [Text],
        chromeBinary :: Maybe Text,
        chromeExtensions :: Maybe [Text]
      }
  | FirefoxOptions
      { firefoxArgs :: Maybe [Text],
        firefoxBinary :: Maybe Text,
        firefoxProfile :: Maybe Text
      }
  | SafariOptions
      { safariAutomaticInspection :: Maybe Bool,
        safariAutomaticProfiling :: Maybe Bool
      }
  deriving (Show, Generic)

-- ToJSON Instance for VendorSpecific
instance ToJSON VendorSpecific where
  toJSON :: VendorSpecific -> Value
  toJSON vs =
    object $ catMaybes props
    where
      props = case vs of
        ChromeOptions {chromeArgs, chromeBinary, chromeExtensions} ->
          [ opt "args" chromeArgs,
            opt "binary" chromeBinary,
            opt "extensions" chromeExtensions
          ]
        FirefoxOptions {firefoxArgs, firefoxBinary, firefoxProfile} ->
          [ opt "args" firefoxArgs,
            opt "binary" firefoxBinary,
            opt "profile" firefoxProfile
          ]
        SafariOptions {safariAutomaticInspection, safariAutomaticProfiling} ->
          [ opt "automaticInspection" safariAutomaticInspection,
            opt "automaticProfiling" safariAutomaticProfiling
          ]

instance FromJSON VendorSpecific where
  parseJSON :: Value -> Parser VendorSpecific
  parseJSON = withObject "VendorSpecific" $ \v -> do
    let chromeOptions = ChromeOptions <$> v .:? "args" <*> v .:? "binary" <*> v .:? "extensions"
        firefoxOptions = FirefoxOptions <$> v .:? "args" <*> v .:? "binary" <*> v .:? "profile"
        safariOptions = SafariOptions <$> v .:? "automaticInspection" <*> v .:? "automaticProfiling"
    chromeOptions <|> firefoxOptions <|> safariOptions

-- ToJSON Instances
instance ToJSON Capabilities where
  toJSON :: Capabilities -> Value
  toJSON (Capabilities name version platform acceptInsecure pageLoad proxy timeouts strictFile unhandledPrompt vendor) =
    object $
      [ "browserName" .= toJSON name,
        "browserVersion" .= version,
        "platformName" .= toJSON platform,
        "acceptInsecureCerts" .= acceptInsecure,
        "pageLoadStrategy" .= toJSON pageLoad,
        "proxy" .= proxy,
        "timeouts" .= timeouts,
        "strictFileInteractability" .= strictFile,
        "unhandledPromptBehavior" .= toJSON unhandledPrompt
      ]
        <> vendorSpecificToJSON vendor

vendorSpecificToJSON :: Maybe VendorSpecific -> [Pair]
vendorSpecificToJSON = maybe [] vendorSpecificToJSON'
  where
    vendorSpecificToJSON' :: VendorSpecific -> [Pair]
    vendorSpecificToJSON' vs = [(fromText (propName vs), toJSON vs)]

    propName :: VendorSpecific -> Text
    propName = \case
      ChromeOptions {} -> "goog:chromeOptions"
      FirefoxOptions {} -> "moz:firefoxOptions"
      SafariOptions {} -> "safari:options"

instance ToJSON Proxy where
  toJSON :: Proxy -> Value
  toJSON
    Proxy
      { proxyType,
        proxyAutoconfigUrl,
        ftpProxy,
        httpProxy,
        sslProxy,
        noProxy
      } =
      object $
        [ "proxyType" .= proxyType,
          "proxyAutoconfigUrl" .= proxyAutoconfigUrl,
          "ftpProxy" .= ftpProxy,
          "httpProxy" .= httpProxy,
          "sslProxy" .= sslProxy
        ]
          <> ["noProxy" .= noProxy | isJust noProxy]

instance ToJSON Timeouts where
  toJSON :: Timeouts -> Value
  toJSON Timeouts {..} =
    object
      [ "script" .= script,
        "pageLoad" .= pageLoad,
        "implicit" .= implicit
      ]

-- ToJSON Instances for Custom Types
instance ToJSON UnhandledPromptBehavior where
  toJSON :: UnhandledPromptBehavior -> Value
  toJSON = \case
    Dismiss -> "dismiss"
    Accept -> "accept"
    DismissAndNotify -> "dismiss and notify"
    AcceptAndNotify -> "accept and notify"
    Ignore -> "ignore"

instance ToJSON PageLoadStrategy where
  toJSON :: PageLoadStrategy -> Value
  toJSON = \case
    None -> "none"
    Eager -> "eager"
    Normal -> "normal"

instance ToJSON BrowserName where
  toJSON :: BrowserName -> Value
  toJSON = \case
    Chrome -> "chrome"
    Firefox -> "firefox"
    Safari -> "safari"
    Edge -> "edge"
    InternetExplorer -> "internet explorer"

instance ToJSON PlatformName where
  toJSON :: PlatformName -> Value
  toJSON = \case
    Windows -> "windows"
    Mac -> "mac"
    Linux -> "linux"
    Android -> "android"
    IOS -> "ios"

instance FromJSON UnhandledPromptBehavior where
  parseJSON :: Value -> Parser UnhandledPromptBehavior
  parseJSON = withText "UnhandledPromptBehavior" $ \case
    "dismiss" -> return Dismiss
    "accept" -> return Accept
    "dismiss and notify" -> return DismissAndNotify
    "accept and notify" -> return AcceptAndNotify
    "ignore" -> return Ignore
    _ -> fail "Invalid UnhandledPromptBehavior"

instance FromJSON PageLoadStrategy where
  parseJSON :: Value -> Parser PageLoadStrategy
  parseJSON = withText "PageLoadStrategy" $ \case
    "none" -> return None
    "eager" -> return Eager
    "normal" -> return Normal
    _ -> fail "Invalid PageLoadStrategy"

instance FromJSON BrowserName where
  parseJSON :: Value -> Parser BrowserName
  parseJSON = withText "BrowserName" $ \case
    "chrome" -> return Chrome
    "firefox" -> return Firefox
    "safari" -> return Safari
    "edge" -> return Edge
    "internet explorer" -> return InternetExplorer
    _ -> fail "Invalid BrowserName"

instance FromJSON PlatformName where
  parseJSON :: Value -> Parser PlatformName
  parseJSON = withText "PlatformName" $ \case
    "windows" -> return Windows
    "mac" -> return Mac
    "linux" -> return Linux
    "android" -> return Android
    "ios" -> return IOS
    _ -> fail "Invalid PlatformName"

-- FromJSON Instances for Data Structures
instance FromJSON Capabilities where
  parseJSON :: Value -> Parser Capabilities
  parseJSON = withObject "Capabilities" $ \v ->
    Capabilities
      <$> v .: "browserName"
      <*> v .:? "browserVersion"
      <*> v .:? "platformName"
      <*> v .:? "acceptInsecureCerts"
      <*> v .:? "pageLoadStrategy"
      <*> v .:? "proxy"
      <*> v .:? "timeouts"
      <*> v .:? "strictFileInteractability"
      <*> v .:? "unhandledPromptBehavior"
      <*> parseVendorSpecific v

parseVendorSpecific :: Object -> Parser (Maybe VendorSpecific)
parseVendorSpecific v =
  v
    .:? "goog:chromeOptions"
    <|> v
    .:? "moz:firefoxOptions"
    <|> v
    .:? "safari:options"

instance FromJSON Proxy where
  parseJSON :: Value -> Parser Proxy
  parseJSON = withObject "Proxy" $ \v ->
    Proxy
      <$> v .: "proxyType"
      <*> v .:? "proxyAutoconfigUrl"
      <*> v .:? "ftpProxy"
      <*> v .:? "httpProxy"
      <*> v .:? "sslProxy"
      <*> v .:? "noProxy"

instance FromJSON Timeouts where
  parseJSON :: Value -> Parser Timeouts
  parseJSON = withObject "Timeouts" $ \v ->
    Timeouts
      <$> v .:? "script"
      <*> v .:? "pageLoad"
      <*> v .:? "implicit"