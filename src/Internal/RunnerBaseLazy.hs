{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError)
import Data.Aeson
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses (Address(..))

data IsRoot

data NonRoot

data SuiteItem r hi effs t where
  Root ::
    { rootElms :: [SuiteItem NonRoot hi effs t]
    } ->
    SuiteItem IsRoot hi effs t
  Tests ::
    { tests :: t
    } ->
    SuiteItem NonRoot hi effs t
  BeforeAll ::
    { title :: Text,
      bHook :: Sem effs o,
      bhElms :: [Address -> o -> SuiteItem NonRoot o effs t]
    } ->
    SuiteItem NonRoot hi effs t
  BeforeEach ::
    { title :: Text,
      bHook :: Sem effs o,
      bhElms :: [Address -> o -> SuiteItem NonRoot o effs t]
    } ->
    SuiteItem NonRoot hi effs t
  AfterAll ::
    { title :: Text,
      aHook :: Sem effs (),
      ahElms :: [Address -> hi -> SuiteItem NonRoot hi effs t]
    } ->
    SuiteItem NonRoot hi effs t
  AfterEach ::
    { title :: Text,
      aHook :: Sem effs (),
      ahElms :: [Address -> hi -> SuiteItem NonRoot hi effs t]
    } ->
    SuiteItem NonRoot hi effs t
  Group ::
    { title :: Text,
      gElms :: [SuiteItem NonRoot hi effs t]
    } ->
    SuiteItem NonRoot hi effs t

instance Functor (SuiteItem r hi effs) where
  fmap :: (a -> b) -> SuiteItem r hi effs a -> SuiteItem r hi effs b
  fmap f si =
    let f''' :: (a' -> b') -> (Address -> c -> SuiteItem r hi' effs a') -> (Address -> c -> SuiteItem r hi' effs b')
        f''' f1 f2 = \a -> (f1 <$>) . f2 a

        f'' :: (a' -> b') -> [Address -> c -> SuiteItem r hi' effs a'] -> [Address -> c -> SuiteItem r hi' effs b']
        f'' fi l = f''' fi <$> l
     in case si of
          Tests a -> Tests $ f a
          BeforeAll title' bHook bhElms -> BeforeAll title' bHook (f'' f bhElms)
          BeforeEach title' bHook bhElms -> BeforeEach title' bHook (f'' f bhElms)
          AfterAll title' aHook ahElms -> AfterAll title' aHook (f'' f ahElms)
          AfterEach title' aHook ahElms -> AfterEach title' aHook (f'' f ahElms)
          Group {title = t, gElms} -> Group t $ (f <$>) <$> gElms
          Root elms' -> Root $ (f <$>) <$> elms'