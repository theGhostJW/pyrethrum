module RunnerBase
  ( AddressedElm (..),
    Domain,
    ItemRunner,
    TestSuite,
    LRB.SuiteItem (..),
    LRB.IsRoot,
    LRB.NotRoot,
    Test (..),
    GenericResult (..),
    concatTests,
    emptyDomain,
    groupAddresses,
    groupName,
    push,
    querySuite,
  )
where

import Common (FilterErrorType, FrameworkError, HookCardinality (..))
import Data.Aeson
import Data.Stack
import Internal.RunnerBaseLazy as LRB
import Polysemy
import Polysemy.Error
import Pyrelude
  ( Applicative ((<*>)),
    Bool (..),
    Category ((.)),
    Either,
    Eq (..),
    Maybe (..),
    Monad ((>>=)),
    Ord (..),
    Ordering (..),
    Show,
    Text,
    error,
    fromJust,
    fromMaybe,
    not,
    otherwise,
    uu,
    ($),
    (&&),
    (<$>),
    (?),
    (||), isNothing
  )
import RunElementClasses

type ItemRunner e as ds i hi tc rc pd effs =
  rc -> pd -> hi -> Test e tc rc hi i as ds effs -> i -> Sem effs ()

type TestSuite e tc rc effs a =
  (forall hi i as ds. (Show i, Show as, Show ds) => hi -> Test e tc rc hi i as ds effs -> a) -> SuiteItem IsRoot () effs [a]

--  (forall hi i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, Show as, Show ds, Show i, ToJSON i) => Test e tc rc hi i as ds effs -> a) -> SuiteItem () effs [a]

data GenericResult tc rslt = TestResult
  { configuration :: tc,
    results :: Either FilterErrorType [rslt]
  }
  deriving (Show)

newtype Domain = Domain {un :: Stack Text} deriving (Show)

instance Eq Domain where
  (==) :: Domain -> Domain -> Bool
  Domain sl == Domain sr =
    let subsEq :: Maybe (Stack Text, Text) -> Maybe (Stack Text, Text) -> Bool
        subsEq ml mr = isNothing ml && isNothing mr || fromMaybe False (do
          (sl', txtl) <- ml
          (sr', txtr) <- mr
          Just $ txtl == txtr && stacksEq sl' sr')

        stacksEq :: Stack Text -> Stack Text -> Bool
        stacksEq ssl ssr = subsEq (stackPop ssl) (stackPop ssr)
     in stacksEq sl sr

instance Ord Domain where
  compare (Domain l) (Domain r) =
    let stackCompare sl sr
          | lempty && rempty = EQ
          | lempty && not rempty = LT
          | not lempty && rempty = GT
          | otherwise =
            let (sl', ltxt) = fromJust $ stackPop sl
                (sr', rtxt) = fromJust $ stackPop sr
                txtCompare = compare ltxt rtxt
             in txtCompare /= EQ ? txtCompare $ stackCompare sl' sr'
          where
            lempty = stackIsEmpty sl
            rempty = stackIsEmpty sr
     in stackCompare l r

push :: Domain -> Text -> Domain
push d t = Domain $ stackPush (un d) t

emptyDomain :: Domain
emptyDomain = Domain stackNew

data AddressedElm a = AddressedElm
  { domain :: Domain,
    element :: a
  }
  deriving (Show)

queryElm' :: forall r hi effs a. (a -> Text) -> Domain -> SuiteItem r hi effs [a] -> [AddressedElm a]
queryElm' getTitle domain =
  let badCall f = f $ error "Bad param - this param should never be called"
      newStack = push domain
   in \case
        Group {title = t, gElms} -> gElms >>= queryElm' getTitle (newStack t)
        Tests {tests} -> (\t -> AddressedElm (newStack (getTitle t)) t) <$> tests
        -- beforeHook, afterHook and root do not contribute to the domain
        BeforeHook {title = t, bhElms} -> bhElms >>= queryElm' getTitle domain . badCall
        AfterHook {title = t, ahElms} -> ahElms >>= queryElm' getTitle domain . badCall
        Root {rootElms} -> rootElms >>= queryElm' getTitle domain

querySuite :: forall hi effs a. (a -> Text) -> SuiteItem IsRoot hi effs [a] -> [AddressedElm a]
querySuite gt = queryElm' gt emptyDomain

-- queryElm :: forall hi effs a. SuiteItem hi effs [a] -> [a]
-- queryElm =
--   let hkElms = (queryElm . (\f -> f $ error "Bad param - this param should never be accessed") =<<)
--    in \case
--         Tests {tests} -> tests
--         BeforeHook {bhElms} -> hkElms bhElms
--         AfterHook {ahElms} -> hkElms ahElms
--         Group {gElms} -> queryElm =<< gElms

{-
TODO
 SuiteItem Update
  DONE - Add Hook
  DONE - Add TestSuite Tests
    * Thread Hook Output to Subelements (GADT)
      * get compiling
      * utilise input - ie change test runner
    * Update Tests
    * Explicit Hook connstructors
    * Update Tests
    * Query static data - items / checks / Config / Known Defects on Checks / Effects esp Hook effects how to query
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

concatTests :: SuiteItem r hi effs t -> [t]
concatTests = uu

-- let
--   concat' ts = mconcat $ concatTests <$> ts
-- in
--   \case
--     (Tests f) -> [f]
--     (BeforeHook _ _ _ ts) -> concat' ts
--     (AfterHook _ _ _ ts) -> concat' ts
--     (Group _ ts) -> concat' ts

groupName :: SuiteItem r hi effs a -> Maybe Text
groupName = \case
  Tests _ -> Nothing
  BeforeHook {} -> Nothing
  AfterHook {} -> Nothing
  Root {} -> Nothing
  Group t _ -> Just t

groupAddresses' :: [Text] -> Text -> SuiteItem r hi effs a -> [Text]
groupAddresses' accum root el = uu

-- let
--   delim = "."

--   appendDelim :: Text -> Text -> Text
--   appendDelim p s = p <> (null p || null s ? empty $ delim) <> s

--   childAddresses :: [SuiteItem o effs a] -> [Text]
--   childAddresses  se = mconcat $ groupAddresses' accum root <$> se
-- in
--   case el of
--     Tests _ -> accum

--     BeforeHook _ _ _ subElems -> childAddresses subElems

--     AfterHook _ _ _ subElems -> childAddresses subElems

--     Group t subElems ->
--       let
--         address = appendDelim root t
--       in
--         address : mconcat (groupAddresses' accum address <$> subElems)

groupAddresses :: SuiteItem r hi effs a -> [Text]
groupAddresses = groupAddresses' [] ""

-- data up to here want items to have title and validations
-- and config

data Test e tc rc hi i as ds effs = Test
  { config :: tc,
    items :: rc -> [i],
    interactor :: rc -> hi -> i -> Sem effs as,
    parse :: forall psEffs. (Member (Error (FrameworkError e)) psEffs) => as -> Sem psEffs ds
  }
