module PyrethrumDemoTest where

import Core (Checks, OnceBefore, ParseException, ThreadBefore, EachResource, chk)
import PyrethrumDemoPrj

-- import qualified DSL.FileSystemEffect as IOI
-- import qualified DSL.Internal.ApEvent as AE

-- import DSL.FileSystemEffect
import DSL.Internal.ApEvent
import DSL.Out
import Effectful (Eff, IOE, (:>))

-- import Effectful.Error.Static (Error, runError)
import PyrethrumExtras (txt)

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . Log

intHook :: Fixture OnceBefore Int
intHook =
  OnceBefore
    { onceAction = \rc -> pure 1
    }

addIntHook :: Fixture OnceBefore Int
addIntHook =
  OnceBefore'
    { onceParent = intHook
    , onceChildAction =
        \rc i -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

intHook2 :: Fixture ThreadBefore Int
intHook2 = ThreadBefore' addIntHook $ \rc i -> do
  log $ "beforeThread' " <> txt i
  pure $ i + 1

-- eachResource :: Fixture EachResource Int
eachResource =
  EachResource'
  { eachResourceParent = intHook2,
    eachChildSetup = \i rc -> do
      log "eachSetup"
      pure 1
  , eachChildTearDown = \i -> do
      log "eachTearDown"
      pure ()
  }

-- ##################################

test :: TestFixture
test =
  Test $ Full config action parse items

config :: TestConfig
config = TestConfig "test" DeepRegression

data ApState = ApState
  { value :: Int
  , valTxt :: Text
  }

action :: RunConfig -> Item -> Suite ApState
action rc itm = do
  log $ txt itm
  pure $ ApState (itm.value + 1) $ txt itm.value

data DState = DState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Generic)

parse :: ApState -> Either ParseException DState
parse ApState{..} = pure DState{..}

data Item = Item
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks DState
  }
  deriving (Show, Generic)

items :: RunConfig -> [Item]
items =
  const
    [ Item
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks = chk "test" ((== 1) . (.value))
        }
    ]

-- ##################################

test2 :: TestFixture
test2 =
  Test $ Full' intHook2 config2 action2 parse2 items2

config2 :: TestConfig
config2 = TestConfig "test" DeepRegression

data ApState2 = ApState2
  { value :: Int
  , valTxt :: Text
  }

action2 :: Int -> RunConfig -> Item2 -> Suite ApState2
action2 i rc itm = do
  log $ txt itm
  pure $ ApState2 (itm.value + 1 + i) $ txt itm.value

data DState2 = DState2
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Generic)

parse2 :: ApState2 -> Either ParseException DState2
parse2 ApState2{..} = pure DState2{..}

data Item2 = Item2
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks DState2
  }
  deriving (Show, Generic)

items2 :: RunConfig -> [Item2]
items2 =
  const
    [ Item2
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks = chk "test" ((== 1) . (.value))
        }
    ]

-- ##################################

test3 :: TestFixture
test3 =
  Test $
    Full'
      { parent = intHook2
      , config = TestConfig "test" DeepRegression
      , childAction = \i rc itm -> do
          log $ txt itm
          pure $ ApState2 (itm.value + 1 + i) $ txt itm.value
      , parse = \ApState2{..} -> pure DState2{..}
      , items =
          const
            [ Item2
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            ]
      }

-- ##################################

test4 :: TestFixture
test4 =
  Test $
    NoParse
      { config = TestConfig "test" DeepRegression
      , action = \rc itm -> do
          log $ txt itm
          pure $ DState2 (itm.value + 1) $ txt itm.value
      , items =
          const
            [ Item2
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            ]
      }

-- ##################################

test5 :: TestFixture
test5 =
  Test $
    Single' 
      {
       parent = eachResource  
      , config = TestConfig "test" DeepRegression
      , childSingleAction = \i rc -> do
          log $ txt itm
          pure $ DState2 (itm.value + 1) $ txt itm.value
      , checks = chk "test" ((== 1) . (.value))
      }

{-

-- TODO : stubs:

    stub (only function - takes an item and adds "DEBUG ONLY" to the title)",
    stubRaw,
    stubHash (make a hash of props other than id and title: ADFG165ER7 - 10 letter hash - module + item),
    stubAll,
    stubFilter (takes a predicate and stubs all items that match it)
-}
