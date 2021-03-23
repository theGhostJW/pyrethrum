module SuiteValidationTest where

import MockSuite ( happyRun, MyText, happySuite, demoSuit)
import DSL.Interpreter ( minInterpret )
import Pyrelude ( ($), Either, isRight, debug )
import Pyrelude.Test ( chk, Assertion, (...) )
import DSL.LogProtocol ( LogProtocolBase )
import Common ( FrameworkError )
import Runner (groupAddresses)
import ItemRunners (runItem)
import Data.Foldable (Foldable(length))
import Data.Text ( Text )


expectedDemoGroupNames :: [Text]
expectedDemoGroupNames = ["Happy Suite", "Happy Suite.Sub Group", "Happy Suite.Empty Group"] 

unit_demo_group_addresses_count :: Assertion
unit_demo_group_addresses_count = 
  length expectedDemoGroupNames ... length $ groupAddresses demoSuit

unit_demo_group_addresses :: Assertion
unit_demo_group_addresses = 
  expectedDemoGroupNames ... groupAddresses demoSuit

-- >>> happySuiteResult
happySuiteResult :: Either (FrameworkError MyText) ([LogProtocolBase MyText], ())
happySuiteResult = minInterpret happyRun

unit_happy_suit_passes_validation :: Assertion
unit_happy_suit_passes_validation = chk $ isRight $ debug happySuiteResult