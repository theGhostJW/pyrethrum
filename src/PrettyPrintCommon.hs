module PrettyPrintCommon where

import           Pyrelude as P
import Text.Show.Pretty as PP

hdr :: Text -> Text -> Text
hdr l h = l <> " " <> h <> " " <> l

subHeader = hdr "----"
header = hdr "===="
tstHeader = hdr "==="
itrHeader = hdr "=="