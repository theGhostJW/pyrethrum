{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError, HookType)
import Data.Aeson hiding (One)
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses (Address (..))


--  node cardinality
data One
data Many
data Root'


data SuiteItem c hi effs t where
  Root ::
    { rootElms :: [SuiteItem One () effs t]
    } ->
    SuiteItem Root' () effs t
  Group ::
    { title :: Text,
      gElms :: [SuiteItem c' hi effs t]
    } ->
    SuiteItem One hi effs t
  Tests ::
    { tests :: Address -> Sem effs hi -> (hi -> Sem effs ()) -> [t]
    } ->
    SuiteItem Many hi effs t
  BeforeAll ::
    { title :: Text,
      bHook :: hi -> Sem effs ho,
      bhElms :: [SuiteItem c' ho effs t]
    } ->
    SuiteItem One hi effs t
  BeforeEach ::
    { title' :: Text,
      bHook' :: hi -> Sem effs ho,
      bhElms' :: [SuiteItem Many ho effs t]
    } ->
    SuiteItem Many hi effs t
  AfterAll ::
    { title :: Text,
      aHook :: ho -> Sem effs (),
      ahElms :: [SuiteItem c' hi effs t]
    } ->
    SuiteItem One hi effs t
  AfterEach ::
    { title' :: Text,
      aHook' :: hi -> Sem effs (),
      ahElms' :: [SuiteItem Many hi effs t]
    } ->
    SuiteItem Many hi effs t

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