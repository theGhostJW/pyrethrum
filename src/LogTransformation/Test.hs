module LogTransformation.Test where

import Common as C (AppError(..))
import LogTransformation.Common
import Check as CK
import Pyrelude as P
import Pyrelude.IO
import Data.DList as D
import DSL.LogProtocol as LP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L


-- iterationStep ::
--               LineNo                                                                -- lineNo
--               -> TestAccum                                                     -- accum
--               -> LogProtocol                                                        -- parse error or apperror
--               -> (TestAccum, Either LogTransformError (Maybe [Test])) -- (newAccum, err / result)
-- iterationStep lineNo accum@(TestAccum lastPhase stageFailure mRec) lp = uu


-- data TestSummary = IterationSummary {
--                           title :: Text,
--                           address :: TestModule,
--                           config :: A.Value
--                         } deriving (Eq, Show)

-- data TestAccum = TestAccum {
--   title :: Text,
--   address :: TestModule,
--   config :: A.Value
-- }

