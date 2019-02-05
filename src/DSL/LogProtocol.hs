
{-# LANGUAGE PolyKinds #-}

module DSL.LogProtocol where

import           DSL.Common (DetailedInfo, AppError)
import           TestFilter
import           Foundation.Extended
import           TestAndRunConfig
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either
import Data.Aeson.Types
import Data.Aeson.TH
import RunnerBase as RB
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

data LogProtocol a where
  Message :: String -> LogProtocol String
  Message' :: DetailedInfo -> LogProtocol DetailedInfo

  Warning :: String -> LogProtocol String
  Warning' :: DetailedInfo -> LogProtocol DetailedInfo

  IOAction :: String -> LogProtocol String

  Error :: AppError -> LogProtocol AppError
  FilterLog :: forall tc. (Show tc, Eq tc, ToJSON tc, FromJSON tc) => [FilterResult tc] -> LogProtocol tc

  StartRun :: forall rc. (Show rc, Eq rc, ToJSON rc, FromJSON rc) => String -> rc -> LogProtocol rc
  StartGroup :: String -> LogProtocol String
  StartTest :: forall tc. (Show tc, Eq tc, ToJSON tc, FromJSON tc) => TestDisplayInfo tc -> LogProtocol tc
  StartIteration :: TestModule -> Int -> LogProtocol (String, Int) -- iid / test module
  EndIteration :: TestModule -> Int -> String -> LogProtocol (String, Int, String) -- test module / iid / test Info
  EndRun :: forall rc. (Show rc, Eq rc, ToJSON rc, FromJSON rc) => rc -> LogProtocol rc

deriving instance Show (LogProtocol a)
deriving instance Eq a => Eq (LogProtocol a)

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
              deriving (Show, Eq, Enum)
                      
$(deriveJSON defaultOptions ''LPTag)

-- https://stackoverflow.com/a/41207422/5589037
-- https://medium.com/@jonathangfischoff/existential-quantification-patterns-and-antipatterns-3b7b683b7d71 -- pattern 2
-- https://stackoverflow.com/questions/2300275/how-to-unpack-a-haskell-existential-type -- cant be done
-- https://stackoverflow.com/questions/54393198/how-can-i-implement-fromjson-on-a-gadt-with-custom-type-class-constraints -- I shouldn't do this

--data AnyLogProtocol = forall a. AnyLogProtocol (LogProtocol a)

-- applyLPFunc :: forall b. (forall a. LogProtocol a -> b) -> Some LogProtocol -> b
-- applyLPFunc f (Some LogProtocol) = f lp

data Some (t :: k -> *) where
  Some :: t x -> Some t

deriving instance Show (Some LogProtocol)

instance FromJSON (Some LogProtocol) where

  parseJSON :: Value -> Parser (Some LogProtocol)
  parseJSON v@(Object o) =
    let 
      tag :: Maybe LPTag 
      tag = do 
        t <- (HML.lookup "type" o) 
        parseMaybe parseJSON t 

      failMessage :: [Char]
      failMessage = toS $ "Could not parse LogProtocol no type field or type field value is not a member of specified in: " 
                      <> (show (enumList :: [LPTag])) 
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
            FilterLogT -> Some <$> (FilterLog <$> o .: "filterResults")
            StartRunT -> Some <$> (StartRun <$> o .: "title" <*> o .: "runConfig")
            StartGroupT -> Some <$> (StartGroup <$> o .: "header")
            StartTestT -> Some <$> (StartTest <$> o .: "displayInfo")
            StartIterationT -> Some <$> (StartIteration <$> o .: "moduleAddress" <*> o .: "iterationId")
            EndIterationT -> Some <$> (EndIteration <$> o .: "moduleAddress" <*> o .: "iterationId" <*> o .: "tstInfo")
            EndRunT -> Some <$> (EndRun <$> o .: "runConfig")
        )       

  parseJSON wtf = typeMismatch "LogProtocol" wtf

instance ToJSON (Some LogProtocol) where
  toJSON :: Some LogProtocol -> Value
  toJSON = \case 
              Some (Message str) -> object [
                                      "type" .= toJSON MessageT, 
                                      "txt" .= str
                                    ]

              Some (Message' detailedInfo) -> object [
                                              "type" .= toJSON MessageT', 
                                               "info" .= toJSON detailedInfo
                                              ]           
              Some (Warning str) -> object [
                                      "type" .= toJSON WarningT, 
                                      "txt" .= str
                                    ]

              Some (Warning' detailedInfo) -> object [
                                              "type" .= toJSON WarningT', 
                                              "info" .= toJSON detailedInfo
                                              ]

              Some (IOAction str) -> object [
                                       "type" .= toJSON IOActionT, 
                                       "txt" .= str
                                      ]

              Some (DSL.LogProtocol.Error e) -> object [
                                                  "type" .= toJSON ErrorT, 
                                                  "err" .= toJSON e
                                                ]

              Some (FilterLog fList) -> object [
                                         "type" .= toJSON FilterLogT, 
                                         "filterResults" .= toJSON fList
                                         ] 

              Some (StartRun ttle rc) -> object [
                                      "type" .= toJSON StartRunT, 
                                      "title" .= ttle,
                                      "runConfig" .= rc
                                    ] 

              Some (StartGroup header) -> object [
                                            "type" .= toJSON StartGroupT, 
                                            "header" .= header
                                          ] 

              Some (StartTest displayInfo) -> object [
                                      "type" .= toJSON StartTestT, 
                                      "displayInfo" .= displayInfo
                                    ] 

              Some (StartIteration moduleAddress' iterationId) -> object [
                                                                    "type" .= toJSON StartIterationT, 
                                                                    "moduleAddress" .= moduleAddress',
                                                                    "iterartionId" .= iterationId
                                                                  ] 

              Some (EndIteration moduleAddress' iterationId tstInfo) ->  object [
                                                                          "type" .= toJSON EndIterationT, 
                                                                          "moduleAddress" .= moduleAddress',
                                                                          "iterartionId" .= iterationId,
                                                                          "tstInfo" .= tstInfo
                                                                          ] 
              Some (EndRun rc) -> object [
                                    "type" .= toJSON EndRunT, 
                                    "runConfig" .= rc
                                  ]