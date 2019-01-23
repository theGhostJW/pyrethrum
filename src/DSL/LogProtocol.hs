
module DSL.LogProtocol where

import           DSL.Common
import           Foundation.Extended
import           TestAndRunConfig
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HML



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
  
  toJSON :: LogProtocol a -> Value
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
  parseJSON :: Value -> Parser (LogProtocol a)
  parseJSON v@(Object obj) = 
    maybe 
      (fail $ toCharList $ "Could not parse LogProtocol no type field specified in: " <> show v) 
      (\case
        "message" -> undefined -- finish
        _ -> undefined
      )
      (HML.lookup "type" obj)
      

  parseJSON wtf = typeMismatch "LogProtocol" wtf


  {-
  instance FromJSON Command where
    -- First of all we lookup for mandatory key `type`
    parseJSON (Object o) = case HML.lookup "type" o of
        Just (String "command") -> let dt = HML.lookup "data" o
                                   in case HML.lookup "name" o of
            -- Then we lookup for key `name`, to distinguish commands
            Just (String "create") -> createCmd dt
            Just (String "update") -> updateCmd dt
            Just (String "delete") -> CommandDelete <$> o .: "data"
            _                      -> unrecognizedCommand
        _ -> pure NotCommand
        where createCmd Nothing           = missingData
              createCmd (Just (Object d)) = CommandCreate <$> d .: "name" <*> d .: "value"
              createCmd _                 = incorrectData
              updateCmd Nothing           = missingData
              updateCmd (Just (Object d)) = CommandUpdate <$> d .: "id"   <*> d .: "value"
              updateCmd _                 = incorrectData

              missingData         = pure $ WrongArg "Missing mandatory `data` key."
              incorrectData       = pure $ WrongArg "Incorrect data received."
              unrecognizedCommand = pure $ WrongArg "Unrecognized command name."
    parseJSON _ = pure NotCommand
  
  -}