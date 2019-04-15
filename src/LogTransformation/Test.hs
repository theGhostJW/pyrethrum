module LogTransformation.Test where

import Common as C (AppError(..))
import LogTransformation.Common
import LogTransformation.Iteration as I
import RunElementClasses (FilterResult)
import Check as CK
import Pyrelude as P hiding (fail)
import Pyrelude.IO
import Data.DList as D
import DSL.LogProtocol as LP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L

iterationStep ::
              LineNo                                                    -- lineNo
              -> TestAccum                                              -- accum
              -> TestIteration                                          -- parse error or apperror
              -> (TestAccum, Either TestTransformError (Maybe [TestIteration])) -- (newAccum, err / result)
iterationStep lineNo (TestAccum mGroup mLastPhase mRunInfo mCurrentTest) itr =
  let 
    thisTest :: TestRecord
    thisTest = fromMaybe emptyRecord mCurrentTest

    defaultProp :: a -> (TestRecord -> a) -> a
    defaultProp = maybef mCurrentTest

    nxtRec :: Test                   
    nxtRec = case itr of
              i@(Iteration ir) -> 
                let 
                  iSummary = summary ir
                  iIssues = issues iSummary
                  iStatus = I.status iSummary
                  tStats = stats thisTest
                in 
                  Test $ thisTest {
                              title = defaultProp "Unavailable Error In Source Logs" title,
                              address = defaultProp "Unavailable Error In Source Logs" address,
                              status = max (LogTransformation.Test.status thisTest) iStatus,
                              stats = tStats {
                                iterationStatusCounts = incStatusCount (iterationStatusCounts tStats) iStatus,
                                issueCounts = issueCounts tStats <> iIssues
                              },
                              iterationsDesc = ir : iterationsDesc thisTest
                            }

              BoundaryItem iaux -> case iaux of
                                      I.FilterLog fl ->

                                      StartRun RunTitle A.Value | 
                                      EndRun |
                                    
                                      StartGroup GroupTitle |
                                      EndGroup GroupTitle |
                                    
                                      StartTest TestDisplayInfo |
                                      EndTest TestModule 
              I.LineError le -> uu
  in 
    uu

data TestTransformError = LogTransError LogTransformError |
                                IterationTransError TestIteration
                                deriving (Eq, Show)

incStatusCount :: StatusCount -> ExecutionStatus -> StatusCount
incStatusCount sc@StatusCount{..} =
  \case 
    Inconclusive -> sc{inconclusive = inconclusive + 1}
    I.Pass -> sc{pass = pass + 1}
    I.Warning _ -> sc{warning = warning + 1}
    I.Fail _ -> sc{fail = fail + 1}

data StatusCount = StatusCount {
                    inconclusive :: Int,
                    pass :: Int,
                    warning :: Int,
                    fail :: Int
                  }
                  deriving (Eq, Show)

instance Semigroup StatusCount where 
  s0 <> s1 = 
    let 
      plus :: (StatusCount -> Int) -> Int
      plus f = f s0 + f s1 
    in 
      StatusCount {
                    pass = plus pass,
                    warning = plus warning,
                    fail = plus LogTransformation.Test.fail,
                    inconclusive = plus inconclusive
                  } 

instance Monoid StatusCount where 
  mempty = StatusCount 0 0 0 0 

data TestStats = TestStats {
                    iterationStatusCounts :: StatusCount,
                    issueCounts :: Issues
                  }
                  deriving (Eq, Show)

instance Semigroup TestStats where 
  TestStats s0 c0 <> TestStats s1 c1 = TestStats (s0 <> s1) (c0 <> c1)

instance Monoid TestStats where 
  mempty = TestStats mempty mempty

data TestRecord = TestRecord {
                    title :: Text,
                    address :: Text,
                    config :: A.Value,
                    status :: ExecutionStatus,
                    stats :: TestStats,
                    iterationsDesc :: [IterationRecord]
                  } deriving (Eq, Show)

emptyRecord :: TestRecord
emptyRecord = TestRecord {
  title = "",
  address = "",
  config = A.Null,
  status = Inconclusive,
  stats = mempty,
  iterationsDesc = []
}

data Test = Test TestRecord |

              FilterLog [FilterResult] |

              StartRun RunTitle A.Value | 
              EndRun |

              StartGroup GroupTitle |
              EndGroup GroupTitle |

              LineError TestTransformError |

              RunStats TestStats
            deriving (Eq, Show)

data TestAccum = TestAccum {
  group :: Maybe Text,
  lastElement :: Maybe Test,
  runInfo :: TestStats,
  currentRec :: Maybe TestRecord
}