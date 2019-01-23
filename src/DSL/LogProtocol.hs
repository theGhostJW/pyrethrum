
module DSL.LogProtocol where

import           DSL.Common
import           Foundation.Extended
import           TestAndRunConfig
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either


data LogProtocol a where
  Message :: String -> LogProtocol String
  Message' :: DetailedInfo -> LogProtocol DetailedInfo

  Warning :: String -> LogProtocol String
  Warning' :: DetailedInfo -> LogProtocol DetailedInfo

  IOAction :: String -> LogProtocol String

  Error :: AppError -> LogProtocol AppError
  FilterLog :: (Show tc, Eq tc, TestConfigClass tc, ToJSON tc, FromJSON tc) => [Either (FilterRejection tc) tc] -> LogProtocol tc

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

instance ToJSON (LogProtocol a) where
  toJSON = \case 
              Message str -> object [
                                      "type" .= "message", 
                                      "txt" .= str
                                    ]

              Message' detailedInfo -> object [
                                              "type" .= "messagePlus", 
                                               "info" .= toJSON detailedInfo
                                              ]
              
              Warning str -> object [
                                      "type" .= "warning", 
                                      "txt" .= str
                                    ]

              Warning' detailedInfo -> object [
                                              "type" .= "warningPlus", 
                                              "info" .= toJSON detailedInfo
                                              ]

              IOAction str -> object [
                                       "type" .= "ioAction", 
                                       "txt" .= str
                                      ]

              DSL.LogProtocol.Error e -> object [
                                                  "type" .= "error", 
                                                  "err" .= toJSON e
                                                ]

              FilterLog fList -> object [
                                         "type" .= "filterLog", 
                                         "errList" .= toJSON fList
                                         ] 

              StartRun rc -> object [
                                      "type" .= "startRun", 
                                      "runConfig" .= rc
                                    ] 

              StartGroup header -> object [
                                            "type" .= "startGroup", 
                                            "runConfig" .= header
                                          ] 

              StartTest tc -> object [
                                      "type" .= "startTest", 
                                      "testConfig" .= tc
                                    ] 

              StartIteration moduleAddress' iterationId -> object [
                                                                    "type" .= "startIteration", 
                                                                    "moduleAddress" .= moduleAddress',
                                                                    "iterartionId" .= iterationId
                                                                  ] 

              EndIteration moduleAddress' iterationId tstInfo ->  object [
                                                                          "type" .= "endIteration", 
                                                                          "moduleAddress" .= moduleAddress',
                                                                          "iterartionId" .= iterationId,
                                                                          "tstInfo" .= tstInfo
                                                                          ] 
              EndRun rc -> object [
                                    "type" .= "endRun", 
                                    "runConfig" .= rc
                                  ] 

instance FromJSON (LogProtocol a) where 
  parseJSON = undefined
  -- No need to provide a parseJSON implementation.