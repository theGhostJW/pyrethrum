module LogTransformation.Test where

import Common as C (AppError(..))
import LogTransformation.Common
import LogTransformation.Iteration
import RunElementClasses
import Check as CK
import Pyrelude as P
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
              -> (TestAccum, Either IterationTransformError (Maybe [TestIteration])) -- (newAccum, err / result)
iterationStep lineNo (TestAccum mLastPhase mRunInfo mCurrentTest) iter = uu

data IterationTransformError = LogTransError LogTransformError |
                               IterationTransError TestIteration
                               deriving (Eq, Show)

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
                    statusCounts :: StatusCount,
                    counts :: IterationStats
                  }
                  deriving (Eq, Show)

instance Semigroup TestStats where 
  TestStats s0 c0 <> TestStats s1 c1 = TestStats (s0 <> s1) (c0 <> c1)

instance Monoid TestStats where 
  mempty = TestStats mempty mempty

data TestRecord = TestRecord {
                    title :: Text,
                    address :: TestModule,
                    config :: A.Value,
                    status :: ExecutionStatus,
                    stats :: TestStats,
                    iterationsDesc :: [IterationRecord]
                  } deriving (Eq, Show)

data Test = Test TestRecord |

              FilterLog [FilterResult] |

              StartRun RunTitle A.Value | 
              EndRun |

              StartGroup GroupTitle |
              EndGroup GroupTitle |

              LineError IterationTransformError |

              RunStats TestStats
            deriving (Eq, Show)

data TestAccum = TestAccum {
  lastElement :: Maybe Test,
  runInfo :: TestStats,
  currentRec :: Maybe TestRecord
}
