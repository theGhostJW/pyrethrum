module RunnerShared where

import qualified Check      as C
import           Foundation as F
import           ItemClass

data TestItem = TestItem {
  iid    :: Int,
  pre    :: String,
  post   :: String,
  checks :: C.CheckList ValState
} deriving (Show)

type ValState = Int

instance ItemClass TestItem ValState where
  identifier = iid
  whenClause = pre
  thenClause = post
  checkList = checks
