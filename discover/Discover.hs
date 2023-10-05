module Discover where

import qualified WeederLibCopy.Weeder.Main as W
import qualified BasePrelude as P
import qualified WeederLibCopy.Weeder as W

-- $> Discover.discover
discover :: IO ()
discover = do 
   discover



-- $ > runWeeder
runWeeder :: IO ()
runWeeder = do 
   W.main


{-
TODO:
   - get file location of fundyion
   - get types
   - filter for interesting fixtures
   - get dependencies of functions
      - test 1 => dependes on => hook 1
      - hook 1 => depends on => hokk 0
      - hook 0 is root
   - profit

   - https://github.com/ocharles/weeder#readme
   - https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Iface-Ext-Types.html
-}
