module RunnerBase where

import DSL.Ensure
import Common (FilterErrorType, FrameworkError)
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson

type ItemRunner e as ds i tc rc effs = 
    rc -> Test e tc rc i as ds effs -> i -> Sem effs ()

type Suite e tc rc effs a = 
    (forall i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i) => Test e tc rc i as ds effs -> a) -> SuiteItem effs [a]

type Ensurable e effs = Members '[Ensure, Error (FrameworkError e)] effs

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

data SuiteItem effs a =
  Tests {
        -- a list of tests
        tests :: a
        -- eg [IO Either (FrameworkError TestInfo)]
   } |

   Hook {
     location :: HookLocation,
     hook :: Sem effs (),
     subElms :: [SuiteItem effs a]
   } |

   Group {
    title :: Text,
    subElms :: [SuiteItem effs a]
   }
   deriving Functor

groupName :: SuiteItem effs a -> Maybe Text
groupName = \case 
              Tests _ -> Nothing
              Hook {} -> Nothing
              Group t _ -> Just t

groupAddresses' :: [Text] -> Text -> SuiteItem effs a -> [Text]
groupAddresses' accum base re = 
  let
    delim = "."

    appendDelim :: Text -> Text -> Text
    appendDelim p s = p <> (null p || null s ? empty $ delim) <> s
  in
    case re of
      Tests _ -> accum
      Hook _ _ subElems -> 
        fold $ groupAddresses' accum base <$> subElems
      Group t subElems -> 
        let 
          address = appendDelim base  t 
          nxtAccum = snoc accum address
        in 
          nxtAccum <> fold (groupAddresses' nxtAccum address <$> subElems)
        

groupAddresses :: SuiteItem effs a -> [Text]
groupAddresses = groupAddresses' [] "" 

data Test e tc rc i as ds effs = Test {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> i -> Sem effs as,
  prepState :: forall psEffs. (Ensurable e) psEffs => i -> as -> Sem psEffs ds
}