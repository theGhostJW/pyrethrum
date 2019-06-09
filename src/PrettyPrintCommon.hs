module PrettyPrintCommon where

import           Pyrelude as P
import           Data.Yaml as Y
import Text.Show.Pretty as PP
import DSL.LogProtocol
import RunElementClasses
import Common 
import Data.Aeson as A


hdr :: Text -> Text -> Text
hdr l h = l <> " " <> h <> " " <> l

newLn :: Text
newLn = "\n"

indent2 :: Text -> Text
indent2 = indentText 2 

subHeader = hdr "----"
header = hdr "===="
tstHeader = hdr "==="
itrHeader = hdr "=="

groupHeader :: GroupTitle -> Text
groupHeader = groupTitle "Group"

groupFooter :: GroupTitle -> Text
groupFooter = groupTitle "End Group"

groupTitle :: Text -> GroupTitle -> Text
groupTitle hdr' gt = header $ hdr' <> " - " <> unGroupTitle gt

ppAsYaml :: ToJSON a => a -> Text
ppAsYaml = indent2 . ppAeson . toJSON

ppAeson:: Y.Value -> Text
ppAeson val = toS ((getLenient . toS . Y.encode $ val) :: Text)

prettyPrintFilterItem :: FilterResult -> Text
prettyPrintFilterItem FilterResult{..} =
    let
      description :: Text
      description = toString (testModAddress testInfo) <> " - " <> testTitle testInfo
    in
      maybef reasonForRejection
        ("accepted: " <> description)
        (\reason -> "rejected: " <> description <> " - Reason: " <> reason)

ppAesonBlock:: Y.Value -> Text
ppAesonBlock = indent2 . ppAeson

ppStartRun :: RunTitle -> Y.Value -> Text
ppStartRun ttle rc = header ("Test Run - " <> unRunTitle ttle) <> 
        newLn <> "Run Config:" <>
        newLn <> ppAesonBlock rc

ppFilterLog :: [FilterResult] -> Text
ppFilterLog fltrInfos = newLn <> header "Filter Log" <> newLn <>
                        foldl (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)
