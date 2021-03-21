module SuiteValidationTest where

import MockSuite
import DSL.Interpreter
import Pyrelude
import           Pyrelude.Test


suiteResult = minInterpret happyRun

unit_dummy = chk True