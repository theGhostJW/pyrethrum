module LogTransformation.Iteration.PrettyPrintDeleteMe where

import Common
import PrettyPrintCommon
import  DSL.LogProtocol
import  DSL.LogProtocol.PrettyPrint
import  qualified LogTransformation.Iteration as I
import           Pyrelude as P
import qualified Data.Map as M
import Text.Show.Pretty as PP
import RunElementClasses as C
import Check (ResultExpectation(..) , ExpectationActive(..), CheckReport(..), CheckInfo(..), GateStatus(..), classifyResult)
import Data.Yaml as Y
import qualified Data.ByteString.Char8 as B


prettyPrintSerialiseIteration :: I.TestIteration -> B.ByteString
prettyPrintSerialiseIteration =  toS . prettyPrintLogIteration

prettyPrintLogIteration :: I.TestIteration -> Text
prettyPrintLogIteration = \case 
                            I.Iteration ir -> printIteration $ prepForPrinting ir
                            I.LineError e -> showPretty e
                            bi@(I.BoundaryItem be) -> 
                              let 
                                -- just regenerate the logProtocol and reprint in the same way
                                ppLogProtocol = prettyPrintLogProtocol False $ BoundaryLog be
                              in
                                case be of 
                                  FilterLog{} -> ppLogProtocol
                                  StartRun{} -> ppLogProtocol
                                  EndRun -> ppLogProtocol
                                  StartGroup{} -> ppLogProtocol
                                  EndGroup{} -> ppLogProtocol
                                  StartTest{} -> ppLogProtocol
                                  EndTest{} -> ppLogProtocol
                                  StartIteration{} -> ""
                                  EndIteration{} -> ""


printIteration :: PrintIteration -> Text
printIteration PrintIteration {..} = 
  let 
    sumTxt I.IterationSummary {..} = 
      let 
        ItemId tstMod id' = iid
        header' = itrHeader $ unTestModule tstMod <> " " <> txt id'
      in 
        uu
  in
    uu


prepForPrinting :: I.IterationRecord -> PrintIteration
prepForPrinting ir = PrintIteration {
  summary = I.summary ir,
  validation = I.validation ir,
  otherErrors = reverse $ I.otherErrorsDesc ir,
  otherWarnings = reverse $ I.otherWarningsDesc ir,
  item = I.item ir,
  apState = I.apState ir,
  domainState = I.domainState ir,
  rawLog = toList $ I.rawLog ir
}

data PrintIteration = PrintIteration {
  summary :: I.IterationSummary,
  validation :: [CheckReport],
  otherErrors :: [I.IterationError],
  otherWarnings :: [I.IterationWarning],
  item :: Maybe I.ItemInfo,
  apState :: Maybe I.ApStateInfo,
  domainState :: Maybe I.PrepStateInfo,
  rawLog :: [LogProtocol]
} deriving (Eq, Show)
