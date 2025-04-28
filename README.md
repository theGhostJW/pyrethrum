# Pyrethrum

## To Reproduce
1. update [project file](cabal.project) with desired ghc version
2. `cabal build`
3. `cabal repl`
4. `ghci> :l SuiteRuntimeTest`
5. `ghci> unit_nested_thread_pass_fail`


## Affected Versions


| version    | result   |
| ---------- | -------- |
| ghc-9.6.7  | <span style="color:red;font-weight:900;">&#x274C;</span> |
| ghc-9.8.2  | <span style="color:red;font-weight:900;">&#x274C;</span>  |
| ghc-9.8.4  | <span style="color:red;font-weight:900;">&#x274C;</span>  |
| ghc-9.10.1 | &#x2705; |


## Example Failures

### 9.6.7
```bash
~/repos/pyrethrum$ cabal repl
Configuration is affected by the following files:
- cabal.project
Build profile: -w ghc-9.6.7 -O0
In order, the following will be built (use -v for more details):
 - pyrethrum-0.1.0.0 (interactive) (lib) (configuration changed)
Configuring library for pyrethrum-0.1.0.0...
Preprocessing library for pyrethrum-0.1.0.0...
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
*** WARNING: .ghci is writable by someone else, IGNORING!
Suggested fix: execute 'chmod go-w .ghci'
[ 1 of 14] Compiling Check            ( src/Check.hs, interpreted )
[ 2 of 14] Compiling DSL.Internal.ApEvent ( src/DSL/Internal/ApEvent.hs, interpreted )
[ 3 of 14] Compiling DSL.Out          ( src/DSL/Out.hs, interpreted )
[ 4 of 14] Compiling FullSuiteTestTemplate ( test/FullSuiteTestTemplate.hs, interpreted )
[ 5 of 14] Compiling Internal.ThreadEvent ( src/Internal/ThreadEvent.hs, interpreted )
[ 6 of 14] Compiling Internal.RunTimeLogging ( src/Internal/RunTimeLogging.hs, interpreted )
[ 7 of 14] Compiling Core             ( src/Core.hs, interpreted )
[ 8 of 14] Compiling List.Extra       ( src/List/Extra.hs, interpreted )
[ 9 of 14] Compiling OrphanedInstances ( src/OrphanedInstances.hs, interpreted )
[10 of 14] Compiling Prepare          ( src/Prepare.hs, interpreted )
[11 of 14] Compiling Internal.SuiteRuntime ( src/Internal/SuiteRuntime.hs, interpreted )
[12 of 14] Compiling SuiteRuntimeTest ( test/SuiteRuntimeTest.hs, interpreted )
[13 of 14] Compiling TempUtils        ( src/TempUtils.hs, interpreted )
[14 of 14] Compiling Text.Extra       ( src/Text/Extra.hs, interpreted )
Ok, 14 modules loaded.
ghci> :l SuiteRuntimeTest 

<no location info>: warning: [GHC-32850] [-Wmissing-home-modules]
    These modules are needed for compilation but not listed in your .cabal file's other-modules for ‘pyrethrum-0.1.0.0-inplace’ :
        Check
        Core
        DSL.Internal.ApEvent
        FullSuiteTestTemplate
        Internal.RunTimeLogging
        Internal.SuiteRuntime
        Internal.ThreadEvent
        Prepare
Ok, 9 modules loaded.
ghci> 
ghci> unit_nested_thread_pass_fail
panic! (the 'impossible' happened)
  GHC version 9.6.7:
        nameModule
  internal stepDownQStatus_itWK
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/GHC/Utils/Panic.hs:189:37 in ghc:GHC.Utils.Panic
        pprPanic, called at compiler/GHC/Types/Name.hs:329:3 in ghc:GHC.Types.Name
  CallStack (from HasCallStack):
    panic, called at compiler/GHC/Utils/Error.hs:454:29 in ghc:GHC.Utils.Error


Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug

```

### ghc-9.8.2
```bash
~/repos/pyrethrum$ cabal repl
Configuration is affected by the following files:
- cabal.project
Build profile: -w ghc-9.8.2 -O0
In order, the following will be built (use -v for more details):
 - pyrethrum-0.1.0.0 (interactive) (lib) (configuration changed)
Configuring library for pyrethrum-0.1.0.0...
Preprocessing library for pyrethrum-0.1.0.0...
GHCi, version 9.8.2: https://www.haskell.org/ghc/  :? for help
*** WARNING: .ghci is writable by someone else, IGNORING!
Suggested fix: execute 'chmod go-w .ghci'
[ 1 of 14] Compiling Check            ( src/Check.hs, interpreted )
[ 2 of 14] Compiling DSL.Internal.ApEvent ( src/DSL/Internal/ApEvent.hs, interpreted )
[ 3 of 14] Compiling DSL.Out          ( src/DSL/Out.hs, interpreted )
[ 4 of 14] Compiling FullSuiteTestTemplate ( test/FullSuiteTestTemplate.hs, interpreted )
[ 5 of 14] Compiling Internal.ThreadEvent ( src/Internal/ThreadEvent.hs, interpreted )
[ 6 of 14] Compiling Internal.RunTimeLogging ( src/Internal/RunTimeLogging.hs, interpreted )
[ 7 of 14] Compiling Core             ( src/Core.hs, interpreted )
[ 8 of 14] Compiling List.Extra       ( src/List/Extra.hs, interpreted )

src/List/Extra.hs:31:14-19: warning: [GHC-63394] [-Wx-partial]
    In the use of ‘head’
    (imported from Data.List.Extra, but defined in GHC.List):
    "This is a partial function, it throws an error on empty lists. Use pattern matching or Data.List.uncons instead. Consider refactoring to use Data.List.NonEmpty."
   |
31 | head = safel L.head
   |              ^^^^^^
[ 9 of 14] Compiling OrphanedInstances ( src/OrphanedInstances.hs, interpreted )
[10 of 14] Compiling Prepare          ( src/Prepare.hs, interpreted )
[11 of 14] Compiling Internal.SuiteRuntime ( src/Internal/SuiteRuntime.hs, interpreted )
[12 of 14] Compiling SuiteRuntimeTest ( test/SuiteRuntimeTest.hs, interpreted )
[13 of 14] Compiling TempUtils        ( src/TempUtils.hs, interpreted )
[14 of 14] Compiling Text.Extra       ( src/Text/Extra.hs, interpreted )
Ok, 14 modules loaded.
ghci> :l SuiteRuntimeTest

<no location info>: warning: [GHC-32850] [-Wmissing-home-modules]
    These modules are needed for compilation but not listed in your .cabal file's other-modules for ‘pyrethrum-0.1.0.0-inplace’ :
        Check
        Core
        DSL.Internal.ApEvent
        FullSuiteTestTemplate
        Internal.RunTimeLogging
        Internal.SuiteRuntime
        Internal.ThreadEvent
        Prepare
Ok, 9 modules loaded.
ghci>  unit_nested_thread_pass_fail
panic! (the 'impossible' happened)
  GHC version 9.8.2:
        nameModule
  internal stepDownQStatus_ivlJ
  Call stack:
      CallStack (from HasCallStack):
        callStackDoc, called at compiler/GHC/Utils/Panic.hs:191:37 in ghc-9.8.2-6af5:GHC.Utils.Panic
        pprPanic, called at compiler/GHC/Types/Name.hs:341:3 in ghc-9.8.2-6af5:GHC.Types.Name
  CallStack (from HasCallStack):
    panic, called at compiler/GHC/Utils/Error.hs:503:29 in ghc-9.8.2-6af5:GHC.Utils.Error


Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug

```