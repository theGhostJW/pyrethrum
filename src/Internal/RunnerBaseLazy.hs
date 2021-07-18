{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError, HookCardinality (..))
import Data.Aeson
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses



data IsRoot

data NotRoot

data SuiteItem r hi effs t where
  Root ::
    { rootElms :: [SuiteItem NotRoot hi effs t]
    } ->
    SuiteItem IsRoot hi effs t
  Tests ::
    { tests :: t
    } ->
    SuiteItem NotRoot hi effs t
  BeforeHook ::
    { title :: Text,
      cardinality :: HookCardinality,
      bHook :: Sem effs o,
      bhElms :: [o -> SuiteItem NotRoot o effs t]
    } ->
    SuiteItem NotRoot hi effs t
  AfterHook ::
    { title :: Text,
      cardinality :: HookCardinality,
      aHook :: Sem effs (),
      ahElms :: [hi -> SuiteItem NotRoot hi effs t]
    } ->
    SuiteItem NotRoot hi effs t
  Group ::
    { title :: Text,
      gElms :: [SuiteItem NotRoot hi effs t]
    } ->
    SuiteItem NotRoot hi effs t

instance Functor (SuiteItem r hi effs) where
  fmap :: (a -> b) -> SuiteItem r hi effs a -> SuiteItem r hi effs b
  fmap f si =
    let f''' :: (a' -> b') -> (c -> SuiteItem r hi' effs a') -> (c -> SuiteItem r hi' effs b')
        f''' f1 f2 = (f1 <$>) . f2

        f'' :: (a' -> b') -> [c -> SuiteItem r hi' effs a'] -> [c -> SuiteItem r hi' effs b']
        f'' fi l = f''' fi <$> l
     in case si of
          Tests a -> Tests $ f a
          BeforeHook title' cardinality bHook bhElms -> BeforeHook title' cardinality bHook (f'' f bhElms)
          AfterHook title' cardinality aHook ahElms -> AfterHook title' cardinality aHook (f'' f ahElms)
          Group {title = t, gElms} -> Group t $ (f <$>) <$> gElms
          Root elms' -> Root $ (f <$>) <$> elms'