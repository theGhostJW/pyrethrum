module PrettyPrintTest where 

import           PyrethrumExtras.IO as PIO
import AuxFiles
import Data.Aeson.TH
import PrettyPrintCommon
import PyrethrumExtras.Test ((...))

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Example Config %%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data RunConfig = RunConfig {
  runTitle    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)

source = [
    ("k:", "v"),
    ("kkkkk:", "vvvv"),
    ("kvvvvvvv:", "vv"),
    ("k:", "vvvvvvvvvvvvvvvvvv")
  ]

expectedLeft = unlines [
    "  k:        v",
    "  kkkkk:    vvvv",
    "  kvvvvvvv: vv",
    "  k:        vvvvvvvvvvvvvvvvvv"
  ]

expectedRight = unlines [
    "  k:                         v",
    "  kkkkk:                  vvvv",
    "  kvvvvvvv:                 vv",
    "  k:        vvvvvvvvvvvvvvvvvv"
  ]

unit_alignKeyValues_left = expectedLeft ... alignKeyValues False 2 LeftJustify source
unit_alignKeyValues_right = expectedRight ... alignKeyValues False 2 RightJustify source



        