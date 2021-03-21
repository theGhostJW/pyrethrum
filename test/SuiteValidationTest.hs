module SuiteValidationTest where

import MockSuite
import DSL.Interpreter
import Pyrelude
import           Pyrelude.Test
import DSL.LogProtocol
import Common


suiteResult :: Either (FrameworkError MyText) ([LogProtocolBase MyText], ())
suiteResult = minInterpret happyRun

unit_dummy = chk True