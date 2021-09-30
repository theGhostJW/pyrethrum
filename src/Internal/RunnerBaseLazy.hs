{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError)
import Data.Aeson hiding (One)
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses (Address (..))


--  node cardinality
data One
data Many


data SuiteItem c hi ho effs t where
  Root ::
    { rootElms :: [SuiteItem c' () ho effs t]
    } ->
    SuiteItem One () () effs t
  Tests ::
    { tests :: t
    } ->
    SuiteItem Many hi ho effs t
  BeforeAll ::
    { title :: Text,
      bHook :: hi -> Sem effs ho,
      bhElms :: [Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> SuiteItem c' ho ho2 effs t]
    } ->
    SuiteItem One hi ho effs t
  BeforeEach ::
    { title' :: Text,
      bHook' :: hi -> Sem effs ho,
      bhElms' :: [Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> SuiteItem c' ho ho2 effs t]
    } ->
    SuiteItem Many hi ho effs t
  AfterAll ::
    { title :: Text,
      aHook :: ho -> Sem effs (),
      ahElms :: [Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> SuiteItem c' hi ho effs t]
    } ->
    SuiteItem One hi ho effs t
  AfterEach ::
    { title' :: Text,
      aHook' :: ho -> Sem effs (),
      ahElms' :: [Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> SuiteItem Many hi ho effs t]
    } ->
    SuiteItem Many hi ho effs t
  Group ::
    { title :: Text,
      gElms :: [Address -> hi -> (hi -> Sem effs ho) -> (ho -> Sem effs ()) -> SuiteItem c' hi ho effs t]
    } ->
    SuiteItem One hi ho effs t

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