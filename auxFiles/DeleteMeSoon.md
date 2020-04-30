## Context
...

---

## One IDE to Rule Them All
* [HIE](https://github.com/haskell/haskell-ide-engine)
    * [LSP Server](https://microsoft.github.io/language-server-protocol/specification)
    * Provenance ?? I think:
      * Arose from a previous non LSP project by [Alan Zimmerman](https://github.com/alanz) 
      * Built on a wide consensus of contributors at the time
* [ghcide - formally hieCore](https://github.com/digital-asset/ghcide)
    * Shake based highly optimised orchestration / plugin / state management system for building Haskell IDE tooling. Depends on many upstream changes maded for HIE
    * [Neil Mitchell](https://github.com/ndmitchell) [et al. x 48](https://github.com/digital-asset/ghcide/graphs/contributors)
    * from [Announce](http://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html)
      * Ghcide uses a Shake graph based approach to manage the IDE state, allowing a simpler programming model.
      * Ghcide breaks the GHC monad apart, making it easier to do tricks like reusing .hi files

 [HIE](https://github.com/haskell/haskell-ide-engine) <> [ghcide - formally hieCore](https://github.com/digital-asset/ghcide) >>= [haskell language server](https://github.com/haskell/haskell-language-server) is a major rerewrite of [HIE] by both these teams such that HIE will sit on top of ghcide

 [HIE files?](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)
  
* [ghcide Talk](https://www.youtube.com/watch?v=cijsaeWNf2E&list=PLxxF72uPfQVRdAsvj7THoys-nVj-oc4Ss)
* [Announce](http://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html)
* [intero end of life](https://www.reddit.com/r/haskell/comments/dr91dv/the_intero_project_has_reached_its_end_of_life/)

---
## The apparent convergence in effect systems to Polysemy and or eff

[freer](https://gitlab.com/queertypes/freer) >>= [freer-effects](https://github.com/IxpertaSolutions/freer-effects) >>= [freer-simple*](https://github.com/lexi-lambda/freer-simple#readme)

[mtl](https://github.com/haskell/mtl) <> [fused effects](https://github.com/fused-effects/fused-effects) <> [freer-simple](https://github.com/lexi-lambda/freer-simple#readme) >>= [polysemy](https://github.com/polysemy-research/polysemy)

[mtl](https://github.com/haskell/mtl) <> [fused effects](https://github.com/fused-effects/fused-effects) <> [freer-simple](https://github.com/lexi-lambda/freer-simple#readme) <> [polysemy](https://github.com/polysemy-research/polysemy) >>= [eff*](https://github.com/hasura/eff)

*Seeking*
 * expressiveness
 * ease of use (eg. intuitive, low boiler plate, error messages)
 * **performance**

 *Current Comparisons - Current*
  * [fused effects readme](https://github.com/fused-effects/fused-effects#comparison-to-other-effect-libraries)
  * [polysemy readme](https://github.com/polysemy-research/polysemy#overview)
  * [eff readme](https://github.com/hasura/eff#eff--screaming-fast-extensible-effects-for-less--)

---
## Record Dot syntax

[proposal](https://github.com/shayne-fletcher-da/ghc-proposals/blob/record-dot-syntax/proposals/0000-record-dot-syntax.md#23-lexing-and-parsing)
```haskell
{-# LANGUAGE RecordDotSyntax #-}

data Company = Company {name :: String, owner :: Person}
data Person = Person {name :: String, age :: Int}

display :: Company -> String
display c = c.name ++ " is run by " ++ c.owner.name

nameAfterOwner :: Company -> Company
nameAfterOwner c = c{name = c.owner.name ++ "'s Company"}
```

* depends on [the HasField proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0158-record-set-field.rst)
* [a more detailed example](https://github.com/shayne-fletcher-da/ghc-proposals/blob/record-dot-syntax/proposals/0000-record-dot-syntax.md#3-examples)
* [PR](https://github.com/ghc-proposals/ghc-proposals/pull/282)
---

## A Couple of Books

* [Functional Design and Architecture - Alexander Granin](https://graninas.com/functional-design-and-architecture-book/)
  * [local](../../ebooks/Book Draft (6 chapters).pdf)
* [Algebra-Driven Design - Sandy Maguire](https://leanpub.com/algebra-driven-design)
  * [local](../../ebooks/algebra-driven-design.pdf)
---
## An Existential Crisis

* [Old Guide](https://github.com/theGhostJW/zenith-test-complete/blob/master/Documentation/Zenith%20Automation%20Framework.pdf)
    * [local page 41](../../ebooks/ZenithAutomationFramework.pdf)
* [pyrethrum](https://github.com/theGhostJW/pyrethrum)
  * change of mind on choice of prelude
  * change from freer simple -> polysemy
  * polymorphising errors 
  * [injecting interpreters was a bad idea](https://github.com/theGhostJW/pyrethrum/blob/master/src/Runner.hs#L335)
  * and still to come:
    * a [test plan](/testDemoProject\DemoProject\TestCaseList.hs) should be:
      > a recursive data structure
      >> a GADT
      >>> an Effect
  * contradiction between a controller and effects
   
 * [simple test](testDemoProject\DemoProject\Test\Simple.hs)
---

## The GHC Steering Committee Mailing List
* [here it is](https://mail.haskell.org/pipermail/ghc-steering-committee/2020-April/date.html)

---
## StringBeGone - (not new)
* [just call encode-string](https://github.com/theGhostJW/pyrelude/blob/1d0a2a28eb5b69a4608b31a17fa46dec1513b8a2/src/Stringy.hs)
[encode-string](https://hackage.haskell.org/package/encode-string-0.1.0.0/docs/Data-String-Encode.html)

---

[other ideas - Stephen Diehl](http://www.stephendiehl.com/posts/decade.html)