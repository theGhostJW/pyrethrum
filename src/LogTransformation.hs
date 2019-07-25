module LogTransformation where

import Common as C (AppError(..))
import LogTransformation.Common
import LogTransformation.Test
import LogTransformation.Stats
import Check as CK
import Pyrelude as P
import Pyrelude.IO as PIO
import Data.DList as D
import Data.Yaml as Y
import qualified Prelude as PO
import AuxFiles
import OrphanedInstances
import DSL.LogProtocol as LP
import DSL.LogProtocol.PrettyPrint
import Text.Show.Pretty as PP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B hiding (putStrLn)
import qualified Data.ByteString.Lazy as L
import System.IO as S hiding (putStrLn)
import Data.Functor
import DSL.Logger
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Identity
import PrettyPrintCommon as PC
import LogTransformation.PrintLogDisplayElement

-- TODO: update to use streaming library such as streamly

-----------------------------------------------------------------
----------------- Generalised Log Transformation ----------------
-----------------------------------------------------------------

data LogTransformParams accum itm rsltItem m srcFmt snkFmt = LogTransformParams {
  source :: m (Maybe srcFmt),
  sink :: [snkFmt] -> m (),
  reducer:: LineNo -> accum -> Either DeserialisationError itm -> (accum, Maybe [rsltItem]),
  itemDesrialiser :: LineNo -> srcFmt -> Either DeserialisationError itm,
  resultSerialiser :: rsltItem -> snkFmt,    
  linNo :: LineNo,
  accumulator :: accum
}

logTransform :: forall accum itm rsltItem m src snk. Monad m => LogTransformParams accum itm rsltItem m src snk -> m accum
logTransform (LogTransformParams src snk step idser rsltSersr lineNo accum) =
    let 
      localLoop :: LineNo -> accum -> m accum
      localLoop ln accum' = logTransform (LogTransformParams src snk step idser rsltSersr ln accum')

      rsltSink :: [rsltItem] -> m ()
      rsltSink lst = snk $ rsltSersr <$> lst
    in
      do 
        lg <- src

        maybef lg
          (pure accum) -- EOF
          (\bs -> 
            let 
              (nxtAccum, result) = step lineNo accum $ idser lineNo bs
            in
              do
                maybe (pure ()) rsltSink result
                localLoop (LineNo $ unLineNo lineNo + 1) nxtAccum
          )
          
------------------------------------------------------
--------------- DList Test Transformation ------------
------------------------------------------------------

-- run a transformation in the context of 
--                - input: StateT a DList which returns one item until the list is empty
--                - output: WriterT a DList of rsltItem values 
transformDList :: DList srcFmt -> LogTransformParams accum input rsltItem (WriterT (DList snkFmt) (StateT (DList srcFmt) Identity)) srcFmt snkFmt -> (accum, DList snkFmt)
transformDList sourceList = runToList sourceList .  logTransform


------------------------------------------------------
----------------- File Transformation ----------------
------------------------------------------------------

sourceFromHandle :: Handle -> IO (Maybe ByteString) 
sourceFromHandle h = hIsEOF h >>= bool (Just <$> B.hGetLine h) (pure Nothing)

sinkFromHandle :: Handle -> [ByteString] -> IO () 
sinkFromHandle h bs = sequence_ $ B.hPutStrLn h <$> bs

withInputFile :: AbsFile -> (Handle -> IO (Either LogTransformError a)) -> IO (Either LogTransformError a)
withInputFile srcPth fLinesProcessor = 
  do
    hSrc <- safeOpenFile srcPth ReadMode
    eitherf hSrc
      (pure . Left . LogIOError "Openning Source File")
      (\hIn -> fLinesProcessor hIn `finally` hClose hIn)

transformToFile :: forall itm rslt accum.    
                  (LineNo -> accum -> Either DeserialisationError itm -> (accum, Maybe [rslt]))  -- line stepper
                  -> (LineNo -> ByteString -> Either DeserialisationError itm)                   -- a deserialiser for the item
                  -> (rslt -> ByteString)                                                        -- a serialiser for the result
                  -> accum                                                                       -- seed accumulator
                  -> AbsFile                                                                     -- source file
                  -> (forall m. MonadThrow m => AbsFile -> m AbsFile)                            -- dest file calculation                                                          -- seed accumulator
                  -> IO (Either LogTransformError AbsFile)
transformToFile step idser rser seed srcPth destPthFunc =
  let
    processLines :: Handle -> Handle -> IO accum
    processLines hIn hOut = logTransform LogTransformParams {
                                            source = sourceFromHandle hIn,
                                            sink = sinkFromHandle hOut,
                                            reducer = step,
                                            itemDesrialiser = idser,
                                            resultSerialiser = rser,    
                                            linNo = LineNo 1,
                                            accumulator = seed
                                          }

    writeLines :: Handle -> IO (Either LogTransformError AbsFile)
    writeLines hIn = 
      do
        rsltFile <- destPthFunc srcPth
        outHndle <- safeOpenFile rsltFile S.WriteMode 
        eitherf outHndle
          (pure . Left . LogIOError "Openning Output File")
          (\hOut -> processLines hIn hOut `finally` S.hClose hOut $> Right rsltFile)
  in
    withInputFile srcPth writeLines
                  
prettyPrintLogprotocolReducer :: LineNo                 -- lineNo
          -> ()                                         -- accum
          -> Either DeserialisationError LogProtocol    -- Logprotocol
          -> ((), Maybe [Text])                         -- (newAccum, err / result)
prettyPrintLogprotocolReducer _ _ ethLp = ((), Just [either txtPretty (prettyPrintLogProtocol False) ethLp])


---------------------------------------------------------
--------------- Log File Post Processing ----------------
---------------------------------------------------------

prettyPrintTestRun :: RunResults -> AbsFile -> IO (Either LogTransformError AbsFile)
prettyPrintTestRun rr srcJsonIniPath = transformToFile 
                                          (printLogDisplayStep rr)
                                          jsonDeserialiser
                                          (toS . prettyPrintDisplayElement)
                                          emptyIterationAccum
                                          srcJsonIniPath
                                          (-<.> ".full.yaml")

fileStats :: AbsFile -> IO (Either LogTransformError RunResults)
fileStats srcJsonIniPath = 
  let
    processLines :: Handle -> IO (Either LogTransformError StatsAccum)
    processLines hIn = Right <$> logTransform LogTransformParams {
                                              source = sourceFromHandle hIn,
                                              sink = const $ pure (),
                                              reducer = statsStepForReducer,
                                              itemDesrialiser = jsonDeserialiser,
                                              resultSerialiser = yamlSerialiser,    
                                              linNo = LineNo 1,
                                              accumulator = emptyStatsAccum
                                            }
  in
    (runResults <$>) <$> withInputFile srcJsonIniPath processLines

prepareFinalLogs :: AbsFile -> IO ()
prepareFinalLogs srcJsonIniPath = 
  do 
    putStrLn "=== Preparing Final Report ==="
    putStrLn "--- Calculating Stats ---"
    runResults <- fileStats srcJsonIniPath
    eitherf runResults 
      (const $ putStrLn "Error - Problem Encountered Calculating Run Stats - Final Report Cannot Be Generated")
      (
        \rr -> do 
                putStrLn "--- Generating Final Report ---"
                ethFile <- prettyPrintTestRun rr srcJsonIniPath
                putStrLn $ eitherf ethFile
                  (\err ->  "Error - Problem Encountered Generating Report" <> newLn <> txt err)
                  (\rsltPth -> "Report Generated: " <> txt (toFilePath rsltPth) <> newLn)
      )
