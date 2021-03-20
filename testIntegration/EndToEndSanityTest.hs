
module EndToEndSanityTest where 

import Pyrelude
import Pyrelude.IO ()
import Pyrelude.Test as T ()
import AuxFiles ()
import LogTransformation.Common ()
import DSL.LogProtocol (LogProtocolBase)
import DSL.Interpreter ( executeForTest, MinEffs, minInterpret )
import Common ()
import Config ( AppError, LogProtocol )
import DSL.LogProtocol.PrettyPrint ()
import RunElementClasses
import Data.Aeson.TH
import qualified RunnerBase
import Polysemy
import TestFilter
import qualified Pyrelude as P
import Runner as R
import Data.Aeson
import ItemRunners

newtype RunConfig = RunConfig {
  include :: Bool
} deriving (Eq, ToJSON, Show, FromJSON )

instance RunConfigClass RunConfig

instance Titled RunConfig where
  title rc = toS $ show rc

data TestConfig = TestConfig {
  header :: Text,
  address :: TestAddress,
  include :: Bool
}  deriving (Show ,Eq)

$(deriveJSON defaultOptions ''TestConfig)

instance TestConfigClass TestConfig where
  moduleAddress = address

instance Titled TestConfig where
  title = header

type MockTest i as ds effs = RunnerBase.Test MyText TestConfig RunConfig i as ds effs

newtype MyInt = MyInt Int deriving (Show, Generic)

instance ToJSON MyInt where
  toEncoding = genericToEncoding defaultOptions

newtype MyText = MyText Text deriving (Show, Generic, ToJSON)

instance ItemClass MyInt MyText where
  identifier _ =  -999
  whenClause _ =  "pre"
  thenClause _ =  "post"
  checkList = mempty

instance ItemClass MyText MyText  where
  identifier _ =  -999
  whenClause _ =  "pre"
  thenClause _ =  "post"
  checkList = mempty



empti :: a -> [b]
empti = const ([] :: [b])

emptiInteractor :: b -> RunConfig -> a -> Sem effs b
emptiInteractor b _ _ = pure b

emptiParser:: a -> i -> as -> Sem effs a
emptiParser a _ _ = pure a

test1 :: MockTest MyInt Text MyText effs
test1 = RunnerBase.Test {
              config = TestConfig {
                header = "test1",
                address = TestAddress "test1",
                include = True
              },
              items = empti,
              interactor = emptiInteractor "Hello",
              parse = \i -> pure . MyText . txt
          }


test2 :: MockTest MyInt Int MyText effs
test2 = Test {
              config = TestConfig {
                header = "test2",
                address = TestAddress "test2 address",
                include = True
              },
              items = empti,
              interactor = emptiInteractor 1,
              parse = \i -> pure . MyText . txt
            }

test3 :: MockTest MyInt Int MyText effs
test3 = Test {
                config = TestConfig {
                  header = "test3",
                  address = TestAddress "test3 address",
                  include = True
                },
              items = empti,
              interactor = emptiInteractor 3,
              parse = \i -> pure . MyText . txt
            }

test4 :: MockTest MyText Text MyText effs 
test4 = Test {
              config = TestConfig {
                  header = "test4",
                  address = TestAddress "test4 address",
                  include = True
                },
              items = empti,
              interactor = emptiInteractor "Hello",
              parse = \i -> pure . MyText . txt
            }

test5 :: MockTest MyInt Int MyText effs
test5 = Test {
              config = TestConfig {
                  header = "test5",
                  address = TestAddress "test5 address",
                  include = True
                },
              items = empti,
              interactor = emptiInteractor 1,
              parse = \i -> pure . MyText . txt
              -- parse = \i _ -> pure i 
            }


includeFilter :: TestFilter RunConfig TestConfig
includeFilter = TestFilter {
     title = "test must be is enabled",
     predicate = \rc tc -> (include :: TestConfig -> Bool) tc == (include :: RunConfig -> Bool) rc
   }


filters' :: [TestFilter RunConfig TestConfig]
filters' = [includeFilter]


mockSuite :: forall effs a. (forall i as ds. (Show i, Show as, Show ds, ToJSON as, ToJSON ds, ItemClass i ds) => MockTest i as ds effs -> a) -> SuiteItem effs [a]
mockSuite r = 
  R.Group "Filter Suite" [
    Hook BeforeAll (pure ()) [
      Tests [
        r test1,
        r test2,
        r test3
      ]
    ],

    R.Group "Sub Group" [
      Tests [
        r test4,
        r test5
      ]
    ],

    R.Group "Empty Group" [
      Tests []
    ]
      
  ]

runParams :: forall effs. MinEffs MyText effs => RunParams Maybe MyText RunConfig TestConfig effs
runParams = RunParams {
  suite = mockSuite,
  filters = filters',
  itemIds = Nothing,   
  itemRunner = runItem,
  rc = RunConfig True
}

testRun :: forall effs. MinEffs MyText effs => Sem effs ()
testRun = mkRunSem runParams

-- >>> 1 + 1
result :: Either (FrameworkError MyText) ([LogProtocolBase MyText], ())
result = minInterpret testRun


-- fullLog :: IO ([LogProtocol], Either AppError ())
-- fullLog = runToLPList

-- logAllways = True

-- chkFullLog :: (([LogProtocol], Either AppError ()) -> Bool) -> (([LogProtocol], Either AppError ()) -> Text) -> IO ()
-- chkFullLog prd msgFunc = 
--   do 
--     lg <- fullLog
--     let 
--       success' = prd lg 
--     unless (success' && not logAllways) (
--       do 
--         putStrLn (success' ? "----- test log ------" $ "----- test failure ------")
--         toTemp . unlines $ prettyPrintLogProtocol False <$> fst lg 
--         putStrLn "-----------"
--      )
--     chk' (msgFunc lg) success'

-- chkRunRslt :: (Either AppError () -> Bool) -> Text -> IO ()
-- chkRunRslt prd msg =
--     chkFullLog  (prd . snd) (\flg -> msg <> "\n" <> txt (snd flg))

-- chkLog :: ([LogProtocol] -> Bool) -> Text -> IO ()
-- chkLog prd msg =
--     chkFullLog (prd . fst) (const (msg <> " - see linked file for log details"))

-- -- Tests --

-- unit_runs_without_error = chkRunRslt isRight "Error in run result should be Right"

-- unit_records_expected_parser_failures = 
--   let 
--     isPrepFailure :: LogProtocol -> Bool 
--     isPrepFailure = \case 
--                         IterationLog (Run (ParserFailure _ _)) -> True
--                         _ -> False
--   in
--     chkLog (\lg -> 2 == count isPrepFailure lg) "expect 2 prep failures 1 for each Rough test iteration 110"
    
