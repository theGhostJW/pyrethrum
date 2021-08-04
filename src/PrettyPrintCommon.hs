module PrettyPrintCommon where

import           Pyrelude as P
import           Data.Yaml.Pretty as YP
import           Data.Yaml as Y
import DSL.LogProtocol
import RunElementClasses
import Common 
import Data.Aeson as A

newLn :: Text
newLn = "\n"

indent2 :: Text -> Text
indent2 = indentText 2 

ppAsYaml :: ToJSON a => a -> Text
ppAsYaml = indent2 . ppAeson . toJSON

ppAeson:: Y.Value -> Text
ppAeson val = toS ((getLenient . toS . Y.encode $ val) :: Text)

prettyPrintFilterItem :: TestFilterResult -> Text
prettyPrintFilterItem TestFilterResult{..} =
  let
    description :: Text
    description = logInfoAddress testInfo <> " - " <> title testInfo
  in
    maybef reasonForRejection
      ("accepted: " <> description)
      (\reason -> "rejected: " <> description <> " - Reason: " <> reason)

headerLine ::  Bool -> Int -> Bool -> Char -> Text -> Text
headerLine isOutline len wantPrcntChar padChr hdrTxt = 
  let 
    padTxt = P.singleton padChr
    txtLen = length hdrTxt + 2
    sfxLen = ceiling $ fromIntegral (len - txtLen) / 2
    pfxLen = txtLen + sfxLen * 2 > len ? pred sfxLen $ sfxLen
    pfxBase = replicateText pfxLen padTxt
    pfx = wantPrcntChar 
                ? replaceFirst (padTxt <> padTxt) "#%" pfxBase 
                $ replaceFirst padTxt "#" pfxBase
  in 
    pfx <> " " <> hdrTxt <> " " <> replicateText sfxLen padTxt

fullHeader :: Bool -> Char -> Bool -> Text -> Text
fullHeader isOutline padChr wantPrcntChar hdrTxt =
  let 
    headerCharWidth :: Int
    headerCharWidth = max 80 $ length hdrTxt + 4

    line :: Text
    line = "#" <> replicateText (headerCharWidth - 1) (P.singleton padChr) <> newLn
  in
    isOutline ? 
      hdrTxt $
      line <> headerLine isOutline headerCharWidth wantPrcntChar padChr hdrTxt <> newLn <> line

majorHeader :: Bool -> Text -> Text
majorHeader isOutline = fullHeader isOutline '#' False

iterationHeader :: Bool -> Text -> Text
iterationHeader isOutline = fullHeader isOutline '-' True

ppAesonBlock:: Y.Value -> Text
ppAesonBlock = indent2 . ppAeson

data Justification = LeftJustify | RightJustify | None

prettyYamlKeyValues :: Int -> Justification -> A.Value -> Text
prettyYamlKeyValues indentation justification val = 
  let 
    prettyYaml = getLenient . convertString $ encodePretty defConfig val
    parts = breakOn ": " <$> lines prettyYaml
    fixedParts = bimap (<> ": ") (\s -> fromMaybe s $ stripPrefix ": " s) <$> parts
  in
    case val of 
      Object _ -> alignKeyValues False indentation justification fixedParts
      _ -> indent2 prettyYaml <> newLn

alignKeyValues :: Bool -> Int -> Justification -> [(Text, Text)] -> Text
alignKeyValues wantColon indentation justification kvs = 
  let 
    spaces = flip replicateText " "
    trimmedKvs = bimap strip strip <$> kvs 
    (maxLKey, maxLVal) = foldl' (\(kl, vl) -> bimap (max kl . length) (max vl . length)) (0, 0) trimmedKvs
    paddedKvs = (\(k, v) -> (
                              -- key:
                              spaces indentation <> k <> ((wantColon ? ":" $ "") <> " "), 
                              -- value
                              let 
                                kSpaces = spaces (maxLKey - length k)
                              in
                                case justification of 
                                  LeftJustify ->  kSpaces <> v
                                  None -> v
                                  RightJustify -> kSpaces <> spaces (maxLVal - length v) <> v
                              )) <$> trimmedKvs
  in  
    unlines $ uncurry (<>) <$> paddedKvs





      
