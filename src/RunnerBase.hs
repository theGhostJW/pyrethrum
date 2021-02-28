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

type TestSuite e tc rc effs a = 
    (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => Test e tc rc i as ds effs -> a) 
    -> RunElement effs [a]

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

{- RunElement Update
  1. Add Hook
  2. Demo Test Run
  2. Thread Hook Output to Subelements (GADT)
  3. Update Demo
  4. Concurrency
  5. Update Demo
  6. Runner that extracts test items - eg to report known errors
-}

data RunElement effs a =
  Tests {
        -- a list of tests
        tests :: a
        -- eg [IO Either (FrameworkError TestInfo)]
   } |

   Hook {
     location :: HookLocation,
     hook :: Sem effs (),
     subElms :: [RunElement effs a]
   } |

   Group {
    title :: Text,
    subElms :: [RunElement effs a]
   }
   deriving Functor

groupName :: RunElement effs a -> Maybe Text
groupName = \case 
              Tests _ -> Nothing
              Hook {} -> Nothing
              Group t _ -> Just t

groupAddresses' :: [Text] -> Text -> RunElement effs a -> [Text]
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
        

groupAddresses :: RunElement effs a -> [Text]
groupAddresses = groupAddresses' [] "" 

data Test e tc rc i as ds effs = Test {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> i -> Sem effs as,
  prepState :: forall psEffs. (Ensurable e) psEffs => i -> as -> Sem psEffs ds
}