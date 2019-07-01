module PrettyPrintCommon where

import           Pyrelude as P
import           Data.Yaml.Pretty as YP
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
ppStartRun ttle rc = majorHeader (unRunTitle ttle) <> 
                      newLn <> newLn <> "Run Config:" <>
                      newLn <> ppAesonBlock rc

ppFilterLog :: [FilterResult] -> Text
ppFilterLog fltrInfos = newLn <> header "Filter Log" <> newLn <>
                        foldl' (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)


headerLine :: Int -> Bool -> Char -> Text -> Text
headerLine len wantPrcntChar padChr hdrTxt = 
  let 
    padTxt = P.singleton padChr
    txtLen = length hdrTxt + 2
    sfxLen = ceiling $ fromIntegral (len - txtLen ) / 2
    pfxLen = txtLen + sfxLen * 2 > len ? pred sfxLen $ sfxLen
    pfxBase = replicateText pfxLen padTxt
    pfx = wantPrcntChar ? replaceFirst (padTxt <> padTxt) (padTxt <> "%") pfxBase $ pfxBase
  in 
    pfx <> " " <> hdrTxt <> " " <> replicateText sfxLen padTxt

fullHeader :: Char -> Bool -> Text -> Text
fullHeader padChr wantPrcntChar hdrTxt = 
  let 
    headerCharWidth = 80
    line = replicateText headerCharWidth (P.singleton padChr) <> newLn
  in
    line <> headerLine headerCharWidth wantPrcntChar padChr hdrTxt <> newLn <> line

majorHeader = fullHeader '#' False

data Justification = LeftJustify | RightJustify

prettyYamlKeyValues :: Int -> Justification -> A.Value -> Text
prettyYamlKeyValues indentation justification a = 
  let 
    parts = breakOn ": " <$> (lines . getLenient . convertString $ encodePretty defConfig a)
    fixedParts = bimap (<> ": ") (\s -> fromMaybe s $ stripPrefix ": " s) <$> parts
  in
    alignKeyValues indentation justification fixedParts


alignKeyValues :: Int -> Justification -> [(Text, Text)] -> Text
alignKeyValues indentation justification kvs = 
  let 
    spaces = flip replicateText " "
    trimmedKvs = bimap strip strip <$> kvs 
    (maxLKey, maxLVal) = foldl' (\(kl, vl) -> bimap (max kl . length) (max vl . length)) (0, 0) trimmedKvs
    paddedKvs = (\(k, v) -> (
                              spaces indentation <> k <> "  ", -- key
                              let 
                                kSpaces = spaces (maxLKey - length k)
                              in
                                case justification of 
                                  LeftJustify ->  kSpaces <> v
                                  RightJustify -> kSpaces <> spaces (maxLVal - length v) <> v
                              )) <$> trimmedKvs
  in  
    unlines $ uncurry (<>) <$> paddedKvs





      
