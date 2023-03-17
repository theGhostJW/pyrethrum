
### Questions 
* lock file vs config stacktoCabal
* ghcid eval
* how lock file works with other libs
* v2 / cabal new
* hpack
## Suit runtime tests

### pyrethrum platform 
* cicd
* libraries and methodology
  * pyrelude
  * deferred validation
  * posthoc failure analysis and defect reconciliation 
### Properties
* check count of errors vs expected
* verify treeshake
* check release - may be done
* check for overlapping events - may already be done
* check tests strat inside the right fixture - may be done
* think about locs
* make hooks maybes
* onceHookReleases must block otherwise:
  ```
   2 threads oh 0.2 could be execute in thread 2 before 
   ohr 0.1 is released, if they use the same resource this 
   could be a problem
   oh
     oh  0.1
     ohr 0.1
     oh  0.2
     ohr 0.2
   ohr
  ```
* hooks / hookrelease should be Maybe - want to do after withou before
  * how would nesting work?

* generalise hooks? - might not work  nodeBracket frequency (Every | Thread | Once) (acc)
* genrative tests - try validity??


## Run Construction
* switch to effectful
  * data Test e tc rc hd i as ds effs = Test
  * data TestGADIT e tc rc hd effs where 
* replace basic effects
* prenode to effecful ??
* connect to suiteruntime
* generate run
  * ux test suite maintenance
  * ux type errors / exceptions

## Checks and Known Errors
## Checks
* remove all concepts of error reconcilliation
* change guard to ensure 
### PostHoc Error 
* runConfig selector
* fixture selector
* test selector
  * parsed item id \i @ItemType -> bool
  * or untyped AESON - use seekOpt -> false if nothing
* errorStatus
  * \rc fx tst -> Active | Inactive | Intermittent | Ignore
* matcher
  * \rc fx tst fullResult err -> Active | Inactive | Intermittent | Ignore
* reconcile multiple files


## Pyrelude
* fix unit_subDirFromBaseDir_finds_test_dir :: Assertion
* fix unit_subDirFromBaseDir_finds_correct_temp :: Assertion
* chkFalse'
* format on debug'
* profiling
* Reinstate Pyrelude (later)
  * swap out stm and friends for unlift io
  * text extra and list extra
  * reexport other modules such as unsafe
  * swap out file functions for filepath and filepathIO (check abstract filepath)
  * default - writefile is not text based
  * import Data.Text.IO (writeFile, putStrLn)
  * update cabal in pyrethrum
-- 

## Other
* move TestHook and TestHookRelease to bracket
* think about hooks
  * mismatch between logical location and tests that need hook
    * its a monad
    * hooks and fixtures
    * hook types
      * instance
      * thread
      * singleton
      * ----
      * once
      * onceThread
      * instance
      * ----
      * suite
      * fixture
      * test
      * ----
      * some combos will not be valid or redudundant
      * what to do when hooks are out of order?
        * instanceTestHook
          * fixtureOnceThread
            * onceHook
              * instanceTestHook
        * consider making hook data a single value?
          * patch - pure function reads patches data from onceContext and ThreadContext into instance context
  * eg. taxcalc tests - how to use
  * some ui - need driver
  * some need mysql rolled over
  * tests inseparate demographics folder need same mysql rolled over
  * same folder
* hasCallStack see lib posted on reddit saved to pocket
* REST
* UI connect to PlaWRIGHT
* defecto
* trojan
  * data
  * shrinker
* explain
  * show workings
* look into Stan
* PyGuts
* use flags in cabal file https://github.com/hasura/graphql-engine/blob/master/server/graphql-engine.cabal - optimisation , include test files etc, 
* add testing ghcid task :: added not working exit after first test
* add refreeze task => https://github.com/haskell/cabal/issues/8047#issuecomment-1069647944
* weeder analysis
* flags for context specific optimisation - eg optomise in CI

```haskell
-- TODO - add tests add to pyrelude
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f =
  M.elems . foldl' fld M.empty . reverse
  where
    fld m a =
      M.lookup (f a) m
        & maybe
          (M.insert (f a) [a] m)
          (\as -> M.insert (f a) (a : as) m)
```


# DONE 

## switch to cabal - done
1. hpack - had to dowload change resolver and build and install in path
2. convert package 
   1. https://github.com/hasufell/stack2cabal/releases/tag/v1.0.13 
   2. rename windows binary => add .exe
   3. copy to pyrethrum
   4. generated cabal.project and cabal.project.freeze
   5. put stack2cabal.exe in folder and add to path

3. stackage / freeze files
   1. [How can I have a reproducible set of versions for my dependencies?](https://cabal.readthedocs.io/en/stable/nix-local-build.html#how-can-i-have-a-reproducible-set-of-versions-for-my-dependencies)
   2. https://www.stackage.org/lts-19.2/cabal.config 
   3. https://github.com/haskell/cabal/issues/7556#issuecomment-1120433903
   4. https://github.com/haskell/cabal/issues/8047
   5. https://github.com/haskell/cabal/issues/8059#issuecomment-1076892558
   6. 
4. build => first cabal build success !!!
5. regen hie.yaml : https://github.com/Avi-D-coder/implicit-hie
   1. cabal install implicit-hie - gen-hie > hie.yaml
6. ghcid - https://stackoverflow.com/questions/75600985/how-can-i-use-ghcid-allow-eval-with-cabal - took days :-(
7. watch - not needed at this stage

## Remove Pyrelude - done
* upgrade ghc :: Done
* stack -> cabal :: Done
* remove 
  * maybef :: Done
  * eitherf :: Done
* change uu to error :: Done
  * uu Error -> txt 
-- compile pyrethrum
* remove listlike :: Done
-- compile pyrethrum :: Done
* relude :: Done

## 
* OverloadedRecordDot / NoRecordSelectors
  * Pyrert5hrum :: Done
  * Pyrelude :: Done
* update tastydiscover should not search dist-newstyle :: not needed wasn't taking in when reran
* replace prelude: https://github.com/dnikolovv/practical-haskell/blob/main/replace-prelude/README.md

