## switch to cabal
1. hpack
   - run hpack needs exe
   - clone rep and build with stack - blew up
   - try with cabal - built for ages => blew up with ambiguous occurance exception
   - asn1-types > Access violation in generated code when writing 0x0
   - try download release 0.34.2 
     - try build with stack and cabal => same result
     - try run script in get Hpackhpack gaild
     - download cabal.config from stackage
       - comment try remote-repo line 
       - try cabal-build - cabal-3.6.2.0.exe: Failed to build yaml-0.11.10.0 (which is required by
exe:hpack from hpack-0.34.2) contraint in cabal file seems to be overriding cabal.config  yaml ==0.11.3.0
       - delete yaml contraint from hpack.cabal
         - yaml >=0.10.0 => yaml 
           - Preprocessing library for yaml-0.11.10.0 still ignoring: cabal.config
       - cabal clean
       - cabal build
       - => same result still tryoing to build yaml-0.11.10.0 
     - rename cabal.config => hpack.freeze
       - !! found 3 other instnaces of yaml >=0.10.0 in cabal file
       - looks like freez file needs a project file ?? come back later <-wrong see below>
       - search github for .freeze didnt find any haskell projects 
       - set all yaml veriosion occurances in cabal file
       - failed
         - src\Hpack\Syntax\DependencyVersion.hs:169:11: error:
           Not in scope: data constructor `AnyVersionF'
           Perhaps you meant one of these:
             `AnyVersion' (line 50),
               variable `D.anyVersion' (imported from Distribution.Version),
               variable `any Version' (line 64)
          |
          |           AnyVersionF -> AnyVersionF
      - rename freeze file => cabal.project.freeze
      - ```
          PS C:\hpack-0.34.2> cabal build 
          Warning: C:\hpack-0.34.2\cabal.project.freeze: Unrecognized field
          'remote-repo' on line 7
          cabal-3.6.2.0.exe: Cannot find the program 'ghc'. User-specified path
          'ghc-8.8.3' does not refer to an executable and the program is not on the
          system path.

          PS C:\hpack-0.34.2> cabal build 
          cabal-3.6.2.0.exe: Cannot find the program 'ghc'. User-specified path
          'ghc-8.8.3' does not refer to an executable and the program is not on the
          system path.

      ```
      - fix by commenting out - top 2 lines
      ```
        -- remote-repo: stackage-lts-15.11:http://www.stackage.org/lts-15.11
        -- with-compiler: ghc-8.8.3
      ```
      - cabal build failed 
      ```
       - After searching the rest of the dependency tree exhaustively, these were the
        goals I've had most trouble fulfilling: hpack
      ```
      - comment out hpack from freeze file
      - solver still failing 
      - remove all constraints from .cabal
      - still failing give up - is hpack installed with stack? search for hpack
      - remove bounds in package.yaml run stack2cabal
      - cabal-build
      - could't find compiler
      - remove with-compiler \
      - cabal build => seemed to hang
      - fork master 
      - remove all constraints and set resolver to nightly-2023-02-23
      - stack build => Compiles
      - stack test => runs single failure 
      ```
        Failures:

  test\HpackSpec.hs:55:40: 
  1) Hpack.renderCabalFile is inverse to readCabalFile
       expected: ["../../hpack.cabal"]
        but got: ["../../hpack.cabal", "-- This file has been generated from package.yaml by hpack.", "--", "-- see: https://github.com/sol/hpack", ""]

       To rerun use: --match "/Hpack/renderCabalFile/is inverse to readCabalFile/"

       Randomized with seed 927868982

       Finished in 11.7191 seconds
       546 examples, 1 failure, 3 pending
      ```
        

1. convert package 
   1. https://github.com/hasufell/stack2cabal/releases/tag/v1.0.13 
   2. rename windows binary => add .exe
   3. copy to pyrethrum
   4. generated cabal.project and cabal.project.freeze
   5. put stack2cabal.exe in folder and add to path

2. stackage / freeze files
   1. [How can I have a reproducible set of versions for my dependencies?](https://cabal.readthedocs.io/en/stable/nix-local-build.html#how-can-i-have-a-reproducible-set-of-versions-for-my-dependencies)
   2. https://www.stackage.org/lts-19.2/cabal.config 
   3. https://github.com/haskell/cabal/issues/7556#issuecomment-1120433903
   4. https://github.com/haskell/cabal/issues/8047
   5. 
3. build => first cabal build success !!!
4. regen hie.yaml : https://github.com/Avi-D-coder/implicit-hie
   1. cabal install implicit-hie - gen-hie > hie.yaml
5. ghcid
6. watch


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
* connect to suiteruntime
* generate run
  * ux type errors

## Checks and Known Errors
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

### Checks
* remove all concepts of error reconcilliation
* change guard to Assert

## Pyrelude
* relude
* chkFalse'
* change uu to error
  * uu Error -> txt 

* remove 
  * maybef
  * eitherf
* upgrade ghc
* format on debug'
* try removing crazy cradal (pyrethrum)
* profiling

## Other
* move TestHook and TestHookRelease to bracket
* hasCallStack see lib posted on reddit saved to pocket
* REST
* UI connect to PlaWRIGHT
* defecto
* trojan
  * data
  * shrinker
* explain
  * show workings

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

-- # TODO replace prelude: https://github.com/dnikolovv/practical-haskell/blob/main/replace-prelude/README.md

----
Introduction:

End-to-end testing is an essential part of software development, as it ensures that all aspects of an application work together as expected. However, current test automation frameworks can be complex and difficult to use, and often require extensive knowledge of multiple programming languages. To address this issue, we propose a new end-to-end test automation framework that integrates with Playwright using Haskell.

Background:

Playwright is a powerful tool for automating web browsers, and it allows developers to write tests in multiple languages, including JavaScript, Python, and C#. Haskell, on the other hand, is a functional programming language that is known for its strong type system and expressive syntax. By combining these two technologies, we can create a test automation framework that is both powerful and easy to use.

Objective:

The goal of this proposal is to create an end-to-end test automation framework that utilizes the strengths of both Playwright and Haskell. The framework will provide a simple and intuitive interface for writing tests, while also taking advantage of Haskell's powerful type system to ensure that tests are accurate and reliable.

Features:

    Utilize Playwright for automating web browsers
    Leverage Haskell's strong type system to prevent errors
    Provide a simple and intuitive interface for writing tests
    Support for parallel test execution
    Detailed test reporting
    Integration with popular CI/CD pipeline

Implementation:

The framework will be implemented using Haskell and will utilize the Playwright library for automating web browsers. The framework will provide a simple and intuitive interface for writing tests, and will use Haskell's powerful type system to ensure that tests are accurate and reliable. To support parallel test execution, the framework will use Haskell's parallelism and concurrency features, and will provide detailed test reporting.

The framework will be open-source and will be made available on GitHub for developers to use and contribute to. The framework will be well-documented and will include examples of how to use it. The framework will also integrate with popular CI/CD pipeline.

Conclusion:

By combining the power of Playwright and Haskell, we can create a test automation framework that is both powerful and easy to use. This framework will provide a simple and intuitive interface for writing tests, while also taking advantage of Haskell's strong type system to ensure that tests are accurate and reliable. With the integration with popular CI/CD pipeline, this framework will be a great asset for developers looking to automate their end-to-end testing process.