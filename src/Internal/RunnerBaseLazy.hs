{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError, HookCardinality (..))
import Data.Aeson
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses

data SuiteItem hi effs t where
  Tests ::
    { tests :: t
    } ->
    SuiteItem hi effs t
  BeforeHook ::
    { title :: Text,
      cardinality :: HookCardinality,
      bHook :: Sem effs o,
      bhElms :: [o -> SuiteItem o effs t]
    } ->
    SuiteItem hi effs t
  AfterHook ::
    { title :: Text,
      cardinality :: HookCardinality,
      aHook :: Sem effs (),
      ahElms :: [hi -> SuiteItem hi effs t]
    } ->
    SuiteItem hi effs t
  Group ::
    { title :: Text,
      gElms :: [SuiteItem hi effs t]
    } ->
    SuiteItem hi effs t

instance Functor (SuiteItem hi effs) where
  fmap :: (a -> b) -> SuiteItem hi effs a -> SuiteItem hi effs b
  fmap f si =
    let f''' :: (a' -> b') -> (c -> SuiteItem hi' effs a') -> (c -> SuiteItem hi' effs b')
        f''' f1 f2 = (f1 <$>) . f2

        f'' :: (a' -> b') -> [c -> SuiteItem hi' effs a'] -> [c -> SuiteItem hi' effs b']
        f'' fi l = f''' fi <$> l
     in case si of
          Tests a -> Tests $ f a
          BeforeHook title' cardinality bHook bhElms -> BeforeHook title' cardinality bHook (f'' f bhElms)
          AfterHook title' cardinality aHook ahElms -> AfterHook title' cardinality aHook (f'' f ahElms)
          Group {title = t, gElms} -> Group t $ (f <$>) <$> gElms