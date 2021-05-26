module RunnerBase where

import Common (FilterErrorType, FrameworkError, HookLocation(..))
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson

type ItemRunner e as ds i tc rc effs = 
    rc -> Test e tc rc i as ds effs -> i -> Sem effs ()

type Suite e tc rc effs a = 
    (forall i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i, ToJSON i) => Test e tc rc i as ds effs -> a) -> SuiteItem effs [a]

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

{- 
TODO 
 SuiteItem Update
  DONE - Add Hook     
  Add Suite Tests
  Thread Hook Output to Subelements (GADT)
  Update Tests
  Query static data - items / checks / Config / Known Defects on Checks
  Update Tests
  Concurrency
  Update Tests
  Known Defect on interact / parse
  Exception error handling
  Update Tests
  sample for actions that return values in the context of documentation
  Update log interpretor 
  Runner that extracts test items - eg to report known errors
  Update Tests
  Update Demo
-}


{-  Play Data Structure -}

data SuiteItemGADT i effs t where
  Tests' ::  { 
    tests :: i -> t 
  } -> SuiteItemGADT i effs t

  Hook' :: { 
    hook :: i -> Sem effs o, 
    subElms :: [SuiteItemGADT o effs t] 
  } -> SuiteItemGADT i effs t
  

mkTests :: (Num a, Show a, Enum a) => a -> [Text]
mkTests i = (\ii -> "item:" <> txt (ii + i)) <$> take 10 [1..]

tests' :: SuiteItemGADT Int effs [Text]
tests' = Tests' { tests = mkTests }

hookFunc :: forall effs. () -> Sem effs Int 
hookFunc t = pure 7

-- embed hook with different tests test input 
-- must handle nested hooks
-- hook needs input

suiteSimple :: SuiteItemGADT () effs [Text]
suiteSimple = Hook' {
  hook = hookFunc,
  subElms = [tests']
}

hookInnerFunc :: forall effs. Int -> Sem effs Int 
hookInnerFunc i = pure $ i + i

subHook :: SuiteItemGADT Int effs [Text]
subHook = Hook' {
    hook = hookInnerFunc,
    subElms = [tests']
  }

suiteNested :: forall effs. SuiteItemGADT () effs [Text]
suiteNested = Hook' {
  hook = hookFunc,
  subElms = [subHook]
}

suiteNested2 :: forall effs. SuiteItemGADT () effs [Text]
suiteNested2 = Hook' {
  hook = hookFunc,
  subElms = [
    subHook, 
    tests'
  ]
}

{-  Play Data Structure End -}

data SuiteItem effs t =
  Tests {
      tests :: t
   } |

   Hook {
     title :: Text,
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
      (Hook _ _ _ ts) -> concat' ts
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
      
      Hook _ _ _ subElems -> 
        mconcat $ groupAddresses' accum root <$> subElems
      
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