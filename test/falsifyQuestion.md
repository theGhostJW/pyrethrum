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
  | Fixture
      { tests :: [Spec]
      }
  deriving (Show, Eq)
```

For instances to be valid:
 -  `OnceBefore` templates can parent `OnceBefore` and `Fixture`
 -  `Fixture`s are leaf nodes

Keeping these constraints in mind the generator for these records is as follows:

```haskell
genNode :: GenParams -> Gen Template
genNode gl@GenParams{
  maxDepth, 
  maxDelay, 
  maxBranches, 
  maxTests, 
  passPcnt} =
  frequency
    [ (fixtureWeight, genFixture)
    , (hkWeight, genOnceBefore)
    ]
 where
  hkWeight = 20
  fixtureWeight = 100 - hkWeight * 2
  nxtLimits = gl{maxDepth = maxDepth - 1}
  genSubnodes = list (between (1, maxBranches))
  genOnceSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Once})
  genSpec' = genSpec maxDelay passPcnt
  genOnceBefore = OnceBefore <$> genSpec' <*> genOnceSubnodes
  genFixture = Fixture <$> (list (between (1, maxTests)) $ genSpec')
```

The generator is recursive and changes the limits on each call to ensure that the above constraints aren't violated and the size of the generated tree is bounded. Eg:

```Haskell
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
    
    Logs for complete shrink history:
    
    Step 1
    generated [ Fixture
        { tests =
            [ Spec { delay = 129 , result = Pass }
            , Spec { delay = 10 , result = Pass }
            -- + 19 dataSource
            ]
        }
    , OnceBefore
        { spec = Spec { delay = 28 , result = Pass }
        , subNodes =
            [ Fixture
                { tests =
                    [ Spec { delay = 258 , result = Pass }
                     -- + 10 dataSource
                    ]
                }
            ]
        }
    , Fixture
        { tests =
            [ Spec { delay = 172 , result = Pass }
            -- + 10 dataSource
            ]
        }
    , Fixture
        { tests =
            [ Spec { delay = 182 , result = Fail }
            -- + 13 dataSource
            ]
        }
    ] at CallStack (from HasCallStack):
      genWith, called at test/SuiteRuntimeTest.hs:286:3 in pyrethrum-0.1.0.0-inplace:SuiteRuntimeTest
    
    -- always shrinks to this with no intermediate shrinks
    Step 2
    generated [ Fixture { tests = [ Spec { delay = 0 , result = Pass } ] } ] at CallStack (from HasCallStack):
      genWith, called at test/SuiteRuntimeTest.hs:286:3 in pyrethrum-0.1.0.0-inplace:SuiteRuntimeTest
```
