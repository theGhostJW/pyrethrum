{-# LANGUAGE NoStrictData #-} 

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError, HookCardinality(..))
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson

data SuiteItem hi effs t where
  Tests ::  { 
    tests :: t
  } -> SuiteItem hi effs t

  BeforeHook :: {
     title :: Text,
     cardinality :: HookCardinality,
     bHook :: Sem effs o,
     bhElms :: [o -> SuiteItem o effs t]
  } -> SuiteItem hi effs t

  AfterHook :: {
     title :: Text,
     cardinality :: HookCardinality,
     aHook :: Sem effs (),
     ahElms :: [hi -> SuiteItem hi effs t]
  } -> SuiteItem hi effs t

  Group :: {
    title :: Text,
    gElms :: [SuiteItem hi effs t]
  } -> SuiteItem hi effs t