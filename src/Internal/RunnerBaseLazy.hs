{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError, HookType)
import Data.Aeson
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses (Address (..))


newtype Suite hi effs t = Suite {
  un :: [SuiteItem hi effs t ]
}

data SuiteItem hi effs t where
  Group ::
    { title :: Text,
      gElms :: [SuiteItem hi effs t]
    } ->
    SuiteItem hi effs t
  Tests ::
    { tests :: Address -> Sem effs hi -> Sem effs () -> [t]
    } ->
    SuiteItem hi effs t
  BeforeAll ::
    { title :: Text,
      bHook :: hi -> Sem effs ho,
      bhElms :: [SuiteItem ho effs t]
    } ->
    SuiteItem hi effs t
  BeforeEach ::
    { title' :: Text,
      bHook' :: hi -> Sem effs ho,
      bhElms' :: [SuiteItem ho effs t]
    } ->
    SuiteItem hi effs t
  AfterAll ::
    { title :: Text,
      aHook :: ho -> Sem effs (),
      ahElms :: [SuiteItem hi effs t]
    } ->
    SuiteItem hi effs t
  AfterEach ::
    { title' :: Text,
      aHook' :: hi -> Sem effs (),
      ahElms' :: [SuiteItem hi effs t]
    } ->
    SuiteItem hi effs t

{-
instance Functor (SuiteItem hi ho effs) where
  fmap :: (a -> b) -> SuiteItem hi ho effs a -> SuiteItem hi ho effs b
  fmap f si =
    let f''' :: (a' -> b') -> (Address -> c -> SuiteItem hi' ho' effs a') -> (Address -> c -> SuiteItem hi' ho' effs b')
        f''' f1 f2 = \a -> (f1 <$>) . f2 a

        f'' :: (a' -> b') -> [Address -> c -> SuiteItem hi' ho' effs a'] -> [Address -> c -> SuiteItem hi' ho' effs b']
        f'' fi l = f''' fi <$> l
     in case si of
          Tests a -> Tests $ f a
          BeforeAll title' bHook bhElms -> BeforeAll title' bHook (f'' f bhElms)
          BeforeEach title' bHook bhElms -> BeforeEach title' bHook (f'' f bhElms)
          AfterAll title' aHook ahElms -> AfterAll title' aHook (f'' f ahElms)
          AfterEach title' aHook ahElms -> AfterEach title' aHook (f'' f ahElms)
          Group {title = t, gElms} -> Group t $ f'' f gElms
-}