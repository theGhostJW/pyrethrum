Hello,

I am trying to use [falsify](https://hackage.haskell.org/package/falsify-0.2.0) to write a generator for a recursive record type. The generator appears to work, but the shrinking doesn't. It shrinks straight to the simplest value. The result looks as if [I am generating and discarding values](https://well-typed.com/blog/2023/04/falsify/?utm_source=pocket_reader#selective-functors) but I don't get how, or how to fix it.

A somewhat simplified version of the data / generator is as follows:

The data type is a follows:
```haskell
data Template
  = OnceBefore
      { spec :: Spec
      , subNodes :: [Template]
      }
  | ThreadBefore
      { threadSpec :: ManySpec
      , subNodes :: [Template]
      }
  | EachBefore
      { eachSpec :: ManySpec
      , subNodes :: [Template]
      }
  | Fixture
      { tests :: [Spec]
      }
  deriving (Show, Eq)
```

For instances to be valid:
 -  `OnceBefore` templates can parent `OnceBefore`, `ThreadBefore` and `Fixture`
 -  `ThreadBefore` templates can parent `ThreadBefore` and `Fixture`
 -  `Fixture`s are leaf nodes

Keeping these constraints in mind the generator for these records is as follows:

```haskell
genNode :: GenParams -> Gen Template
genNode gl@GenParams{genStrategy, 
  maxDepth, 
  minHz, 
  maxDelay, 
  maxBranches, 
  maxTests, 
  hookPassPcnt,
  passPcnt} =
  frequency
    [ (fixtureWeight, genFixture)
    , (onceWeight, genOnceBefore)
    , (threadWeight, genThreadBefore)
    ]
 where
  hkWeight = maxDepth < 2 ? 0 $ 10
  onceWeight = minHz > Once ? 0 $ hkWeight
  threadWeight = minHz > Thread ? 0 $ hkWeight
  fixtureWeight = 100 - (onceWeight + threadWeight) * 2
  nxtLimits = gl{maxDepth = maxDepth - 1}
  genSubnodes = list (between (1, maxBranches))
  genOnceSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Once})
  genSpec' = genSpec maxDelay passPcnt
  genOnceBefore = OnceBefore <$> genSpec' <*> genOnceSubnodes
  genThreadSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Thread})
  genManySpec =
    frequency
      [ (10, T.All <$> genSpec')
      , (90, T.PassProb genStrategy (fromIntegral hookPassPcnt) 0 <$>  genDelay maxDelay)
      ]
  genThreadBefore = ThreadBefore <$> genManySpec <*> genThreadSubnodes
  genFixture = Fixture <$> (list (between (1, maxTests)) $ genSpec')

```

The generator is recursive and changes the limits on each call to ensure that the above constraints aren't violated and the size of the generated tree is bounded. Eg:

```Haskell
-- ensure no Once nodes are generated as a Thread subnode because the weight of the genOnce generator is set to 0...
genThreadSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Thread})
ght = minHz > Once ? 0 $ hkWeight


--- on every recursion the max depth is decremented by one
-- when max depth hits 1 only fixtures are generated
nxtLimits = gl{maxDepth = maxDepth - 1}
hkWeight = maxDepth < 2 ? 0 $ 10
```

I am generating a list of these Templates and logging  the shrinking against an always failing test:

```Haskell
genTemplate :: GenParams -> Gen [Template]
genTemplate p = list (between (1, p.maxBranches)) $ genNode p

demoTemplateShrinking :: TestTree
demoTemplateShrinking = testPropertyWith def "Template" $ do
  genWith (Just . ppShow) $ genTemplate genParams
  assert $ FP.alwaysFail
```

The first template generated looks good but then the shrinking always stampedes to the simplest case on the first shrink.

What am I doing wrong here? I'd expect to see much gentler shrinking than this.
```haskell
  Logs for complete shrink history:
    
    Step 1
    generated [ OnceBefore
        { spec = Spec { delay = 7 , result = Pass }
        , subNodes =
            [ Fixture
                { tests =
                    [ Spec { delay = 703 , result = Pass }
                    , Spec { delay = 72 , result = Pass }
                    -- + 18 items
                    ]
                }
            , Fixture
                { tests =
                    [ Spec { delay = 78 , result = Pass }
                    -- + 17 items
                    ]
                }
            , Fixture
                { tests =
                    [ Spec { delay = 29 , result = Pass }
                    -- + 20 items
                    ]
                }
            , Fixture
                { tests =
                    [ Spec { delay = 0 , result = Pass }
                    , Spec { delay = 20 , result = Pass }
                    ]
                }
            ]
        }
    , Fixture
        { tests =
            [ Spec { delay = 170 , result = Pass }
            -- + 2 items
            ]
        }
    , OnceBefore
        { spec = Spec { delay = 61 , result = Pass }
        , subNodes =
            [ Fixture
                { tests =
                    [ Spec { delay = 204 , result = Pass }
                     -- + 17 items
                    ]
                }
            ]
        }
    ] at CallStack (from HasCallStack):
      genWith, called at test/SuiteRuntimeTest.hs:263:3 in pyrethrum-0.1.0.0-inplace:SuiteRuntimeTest
    
    -- step 2 always shrinks to this with no intermediate shrinks
    Step 2
    generated [ Fixture { tests = [ Spec { delay = 0 , result = Pass } ] } ] at CallStack (from HasCallStack):
      genWith, called at test/SuiteRuntimeTest.hs:263:3 in pyrethrum-0.1.0.0-inplace:SuiteRuntimeTest
```
