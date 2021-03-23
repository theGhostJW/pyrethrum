module RunnerBase where

import Common (FilterErrorType, FrameworkError)
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson

type ItemRunner e as ds i tc rc effs = 
    rc -> Test e tc rc i as ds effs -> i -> Sem effs ()


-- type TestPlanBase e tc rc m1 m a effs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => 
--                                                           GenericTest e tc rc i as ds effs -> m1 (m a)) -> [TestGroup m1 m a effs]

-- data TestGroup m1 m a effs =
--   TestGroup {
--         header :: Text,
--         -- occurs once on client before group is run
--         rollover :: PreRun effs,
--         -- occurs once before test iteration is run
--         goHome :: PreRun effs,
--         -- a list of tests
--         tests :: [m1 (m a)]
--         -- eg [IO Either (FrameworkError TestInfo)]
--    }

type Suite e tc rc effs a = 
    (forall i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i) => Test e tc rc i as ds effs -> a) -> SuiteItem effs [a]

data GenericResult tc rslt = TestResult {
  configuration :: tc,
  results :: Either FilterErrorType [rslt]
} deriving Show

data PreRun effs = PreRun {
  runAction :: Sem effs (),
  checkHasRun :: Sem effs Bool
}

doNothing :: PreRun effs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}

data HookLocation = BeforeAll | 
                    AfterAll | 
                    BeforeEach |
                    AfterEach

{- SuiteItem Update
  1. DONE - Add Hook     
  2. Update Tests
  3. Add Suite Tests
  4. Thread Hook Output to Subelements (GADT)
  5. Update Tests
  6. Concurrency
  7. Update Tests
  8. Update log interpretor 
  9. Runner that extracts test items - eg to report known errors
  10. Update Tests
  11. Update Demo

-} 

data SuiteItem effs t =
  Tests {
      tests :: t
   } |

   Hook {
     location :: HookLocation,
     hook :: Sem effs (),
     subElms :: [SuiteItem effs t]
   } |

   Group {
    title :: Text,
    subElms :: [SuiteItem effs t]
   }
   deriving Functor

concatTests :: SuiteItem effs t -> [t]
concatTests = 
  let 
    concat' ts = mconcat $ concatTests <$> ts
  in
    \case
      (Tests t) -> [t]
      (Hook _ _ ts) -> concat' ts
      (Group _ ts) -> concat' ts


groupName :: SuiteItem effs a -> Maybe Text
groupName = \case 
              Tests _ -> Nothing
              Hook {} -> Nothing
              Group t _ -> Just t

groupAddresses' :: [Text] -> Text -> SuiteItem effs a -> [Text]
groupAddresses' accum root el = 
  let
    delim = "."

    appendDelim :: Text -> Text -> Text
    appendDelim p s = p <> (null p || null s ? empty $ delim) <> s
  in
    case el of
      Tests _ -> accum
      
      Hook _ _ subElems -> 
        mconcat $ debug' "HOOK" . groupAddresses' accum root <$> subElems
      
      Group t subElems -> 
        let 
          address = appendDelim root t 
        in 
          address : mconcat (groupAddresses' accum address <$> subElems)
        

groupAddresses :: SuiteItem effs a -> [Text]
groupAddresses = groupAddresses' [] "" 

data Test e tc rc i as ds effs = Test {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> i -> Sem effs as,
  parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
}