module PrettyPrintCommon where

import           Pyrelude as P
import           Data.Yaml as Y
import Text.Show.Pretty as PP


hdr :: Text -> Text -> Text
hdr l h = l <> " " <> h <> " " <> l

subHeader = hdr "----"
header = hdr "===="
tstHeader = hdr "==="
itrHeader = hdr "=="

ppAeson:: Y.Value -> Text
ppAeson val = toS ((getLenient . toS . Y.encode $ val) :: Text)