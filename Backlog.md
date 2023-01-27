## Suit runtime tests

### pyrethrum platform 
* cicd
* libraries and methodology
  * pyrelude
  * deferred validation
  * posthoc failure analysis and defect reconciliation 
### laws
* check fixture hooks are not firing on empty fixtures
* check release
* check for overlapping events - may already be done
* check tests strat inside the right fixture
* think about locs

* try validity??


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
* remove 
  * maybef
  * eitherf
* upgrade ghc
* uu Error -> txt
* format on debug'
* switch to cabal
* try removing crazy cradal (pyrethrum)

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