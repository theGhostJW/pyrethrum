{-# LANGUAGE NoStrictData #-}

module Internal.ExeNodeLazy where
import Polysemy
import Prelude





data RunFixture effs = RunFixture
  { testItems :: [Sem effs ()],
    dummy :: Bool
  }

mkRunFixture :: [Sem effs ()] -> RunFixture effs
mkRunFixture i = RunFixture i True