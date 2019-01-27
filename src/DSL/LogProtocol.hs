
{-# LANGUAGE PolyKinds #-}

module DSL.LogProtocol where

import           DSL.Common (DetailedInfo, AppError, FilterRejection )
import           Foundation.Extended
import           TestAndRunConfig
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

data LogProtocol a where
  Message :: String -> LogProtocol String
  Message' :: DetailedInfo -> LogProtocol DetailedInfo

  Warning :: String -> LogProtocol String
  Warning' :: DetailedInfo -> LogProtocol DetailedInfo

  IOAction :: String -> LogProtocol String

  Error :: AppError -> LogProtocol AppError
  FilterLog :: forall tc. (Show tc, Eq tc, TestConfigClass tc, ToJSON tc, FromJSON tc) => [Either (FilterRejection tc) tc] -> LogProtocol tc

  StartRun :: forall rc. (Show rc, Eq rc, Titled rc, ToJSON rc, FromJSON rc) => rc -> LogProtocol rc
  StartGroup :: String -> LogProtocol String
  StartTest :: forall tc. (Show tc, Eq tc, TestConfigClass tc, ToJSON tc, FromJSON tc) => tc -> LogProtocol tc
  StartIteration :: String -> Int -> LogProtocol (String, Int) -- iid / test module
  EndIteration :: String -> Int -> String -> LogProtocol (String, Int, String) -- test module / test Info
  EndRun :: forall rc. (Show rc, Eq rc, Titled rc, ToJSON rc, FromJSON rc) => rc -> LogProtocol rc

deriving instance Show (LogProtocol a)
deriving instance Eq (LogProtocol a)

-- note can't derive Generic for a GADT need to use GADT otherwise type signatures in 
-- Logger.hs get really ugly -> hand roll JSON instances

data LPTag = MessageT |
              MessageT' |
              WarningT |
              WarningT' |
              IOActionT |
              ErrorT |
              FilterLogT  |
              StartRunT |
              StartGroupT |
              StartTestT |
              StartIterationT |
              EndIterationT |
              EndRunT 
              deriving (Show, Eq, Enum, Bounded)
                      
$(deriveJSON defaultOptions ''LPTag)

-- https://stackoverflow.com/a/41207422/5589037
-- https://stackoverflow.com/questions/2300275/how-to-unpack-a-haskell-existential-type -- cant be done
-- https://medium.com/@jonathangfischoff/existential-quantification-patterns-and-antipatterns-3b7b683b7d71 -- pattern 2

--data AnyLogProtocol = forall a. AnyLogProtocol (LogProtocol a)

-- applyLPFunc :: forall b. (forall a. LogProtocol a -> b) -> Some LogProtocol -> b
-- applyLPFunc f (Some LogProtocol) = f lp

data Some (t :: k -> *) where
  Some :: t x -> Some t

instance FromJSON (Some LogProtocol) where

  parseJSON :: Value -> Parser (Some LogProtocol)
  parseJSON v@(Object o) =
    let 
      tag :: Maybe LPTag 
      tag = do 
        t <- (HML.lookup "type" o) 
        parseMaybe parseJSON t 

      failMessage :: [Char]
      failMessage = toS $ "Could not parse LogProtocol no type field or type fiield value is not a member of specified in: " 
                      <> (mconcat $ show <$> (tagList :: [LPTag])) 
                      <> show v

    in 
      maybef tag 
        (fail failMessage )
        (
          \case 
            MessageT -> Some <$> (Message <$> o .: "txt")
            MessageT' -> Some <$> (Message' <$> o .: "info")
            WarningT -> Some <$> (Warning <$> o .: "txt")
            WarningT' -> Some <$> (Warning' <$> o .: "info")
            IOActionT -> Some <$> (IOAction <$> o .: "txt")
            ErrorT -> Some <$> (DSL.LogProtocol.Error <$> o .: "err")
            FilterLogT -> Some <$> (FilterLog <$> o .: "errList")
            StartRunT -> Some <$> (StartRun <$> o .: "runConfig")
            StartGroupT -> Some <$> (StartGroup <$> o .: "runConfig")
            StartTestT -> Some <$> (StartTest <$> o .: "testConfig")
            StartIterationT -> Some <$> (StartIteration <$> o .: "moduleAddress" <*> o .: "iterationId")
            EndIterationT -> Some <$> (EndIteration <$> o .: "moduleAddress" <*> o .: "iterationId" <*> o .: "tstInfo")
            EndRunT -> Some <$> (EndRun <$> o .: "runConfig")
        )       

  parseJSON wtf = typeMismatch "LogProtocol" wtf

tagList :: Enum a => [a]
tagList = enumFrom $ toEnum 0

instance ToJSON (LogProtocol a) where
  toJSON :: LogProtocol a -> Value
  toJSON = \case 
              Message str -> object [
                                      "type" .= toJSON MessageT, 
                                      "txt" .= str
                                    ]

              Message' detailedInfo -> object [
                                              "type" .= toJSON MessageT', 
                                               "info" .= toJSON detailedInfo
                                              ]           
              Warning str -> object [
                                      "type" .= toJSON WarningT, 
                                      "txt" .= str
                                    ]

              Warning' detailedInfo -> object [
                                              "type" .= toJSON WarningT', 
                                              "info" .= toJSON detailedInfo
                                              ]

              IOAction str -> object [
                                       "type" .= toJSON IOActionT, 
                                       "txt" .= str
                                      ]

              DSL.LogProtocol.Error e -> object [
                                                  "type" .= toJSON ErrorT, 
                                                  "err" .= toJSON e
                                                ]

              FilterLog fList -> object [
                                         "type" .= toJSON FilterLogT, 
                                         "errList" .= toJSON fList
                                         ] 

              StartRun rc -> object [
                                      "type" .= toJSON StartRunT, 
                                      "runConfig" .= rc
                                    ] 

              StartGroup header -> object [
                                            "type" .= toJSON StartGroupT, 
                                            "runConfig" .= header
                                          ] 

              StartTest tc -> object [
                                      "type" .= toJSON StartTestT, 
                                      "testConfig" .= tc
                                    ] 

              StartIteration moduleAddress' iterationId -> object [
                                                                    "type" .= toJSON StartIterationT, 
                                                                    "moduleAddress" .= moduleAddress',
                                                                    "iterartionId" .= iterationId
                                                                  ] 

              EndIteration moduleAddress' iterationId tstInfo ->  object [
                                                                          "type" .= toJSON EndIterationT, 
                                                                          "moduleAddress" .= moduleAddress',
                                                                          "iterartionId" .= iterationId,
                                                                          "tstInfo" .= tstInfo
                                                                          ] 
              EndRun rc -> object [
                                    "type" .= toJSON EndRunT, 
                                    "runConfig" .= rc
                                  ]