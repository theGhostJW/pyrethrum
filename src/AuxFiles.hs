{-# LANGUAGE QuasiQuotes #-}

module AuxFiles where


import Prelude as P
import qualified System.IO as S
import qualified Data.Char as C
import Data.Text

data WantConsole = Console | NoConsole deriving Eq


