{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError)
import Data.Aeson
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses (Address (..))

data SuiteItem hi ho effs t where
  Root ::
    { rootElms :: [SuiteItem () ho effs t]
    } ->
    SuiteItem () () effs t
  Tests ::
    { tests :: t
    } ->
    SuiteItem hi ho effs t
  BeforeAll ::
    { title :: Text,
      bHook :: hi -> Sem effs ho1,
      bhElms :: [Address -> ho1 -> SuiteItem ho1 ho2 effs t]
    } ->
    SuiteItem hi ho effs t
  BeforeEach ::
    { title :: Text,
      bHook :: hi -> Sem effs ho1,
      bhElms :: [Address -> ho1 -> SuiteItem ho1 ho2 effs t]
    } ->
    SuiteItem hi ho effs t
  AfterAll ::
    { title :: Text,
      aHook :: hi -> Sem effs (),
      ahElms :: [Address -> hi -> SuiteItem hi ho effs t]
    } ->
    SuiteItem hi ho effs t
  AfterEach ::
    { title :: Text,
      aHook :: hi -> Sem effs (),
      ahElms :: [Address -> hi -> SuiteItem hi ho effs t]
    } ->
    SuiteItem hi ho effs t
  Group ::
    { title :: Text,
      gElms :: [Address -> hi -> SuiteItem hi ho effs t]
    } ->
    SuiteItem hi ho effs t

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
          Root elms' -> Root $ (f <$>) <$> elms'
-}