{-# LANGUAGE NoStrictData #-}

module Internal.RunnerBaseLazy where

import Common (FilterErrorType, FrameworkError)
import Data.Aeson
import Polysemy
import Polysemy.Error
import Pyrelude
import RunElementClasses (Address (..))

newtype TestSuite hd effs t = TestSuite
  { un :: [SuiteItem hd effs t]
  }

data SuiteItem hd effs t where
  Group ::
    { title :: Text,
      gElms :: [SuiteItem hd effs t]
    } ->
    SuiteItem hd effs t
  --
  Tests ::
    { tests :: Address -> hd -> [t]
    } ->
    SuiteItem hd effs t
  --
  OnceHook ::
    { title :: Text,
      bHook :: hd -> Sem effs hd2,
      hkElms :: [SuiteItem hd2 effs t],
      aHook :: hd2 -> Sem effs ()
    } ->
    SuiteItem hd effs t

{-
instance Functor (SuiteItem hd ho effs) where
  fmap :: (a -> b) -> SuiteItem hd ho effs a -> SuiteItem hd ho effs b
  fmap f si =
    let f''' :: (a' -> b') -> (Address -> c -> SuiteItem hd' ho' effs a') -> (Address -> c -> SuiteItem hd' ho' effs b')
        f''' f1 f2 = \a -> (f1 <$>) . f2 a

        f'' :: (a' -> b') -> [Address -> c -> SuiteItem hd' ho' effs a'] -> [Address -> c -> SuiteItem hd' ho' effs b']
        f'' fi l = f''' fi <$> l
     in case si of
          Tests a -> Tests $ f a
          BeforeAll title' bHook bhElms -> BeforeAll title' bHook (f'' f bhElms)
          BeforeEach title' bHook bhElms -> BeforeEach title' bHook (f'' f bhElms)
          AfterAll title' aHook ahElms -> AfterAll title' aHook (f'' f ahElms)
          AfterEach title' aHook ahElms -> AfterEach title' aHook (f'' f ahElms)
          Group {title = t, gElms} -> Group t $ f'' f gElms
-}