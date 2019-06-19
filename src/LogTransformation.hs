module LogTransformation where

import Common as C (AppError(..))
import LogTransformation.Common
-- pimport LogTransformation.Iteration as I
import LogTransformation.Test
import Check as CK
import Pyrelude as P
import Pyrelude.IO
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
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import System.IO as S
import Data.Functor
import DSL.Logger
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Identity
import PrettyPrintCommon as PC

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

transformToFile :: forall itm rslt accum.    
                  (LineNo -> accum -> Either DeserialisationError itm -> (accum, Maybe [rslt]))   -- line stepper
                  -> (LineNo -> ByteString -> Either DeserialisationError itm)                   -- a deserialiser for the item
                  -> (rslt -> ByteString)                                                        -- a serialiser for the result
                  -> accum                                                                       -- seed accumulator
                  -> AbsFile                                                                     -- source file
                  -> (forall m. MonadThrow m => AbsFile -> m AbsFile)                            -- dest file calculation                                                          -- seed accumulator
                  -> IO (Either LogTransformError AbsFile)
transformToFile step idser rser seed srcPth destPthFunc =
  let
    source :: Handle -> IO (Maybe ByteString) 
    source h = hIsEOF h >>= bool (Just <$> B.hGetLine h) (pure Nothing)
    
    sink :: Handle -> [ByteString] -> IO () 
    sink h bs = sequence_ $ B.hPutStrLn h <$> bs
    
    processLines :: Handle -> Handle -> IO accum
    processLines hIn hOut = logTransform  LogTransformParams {
                                            source = source hIn,
                                            sink = sink hOut,
                                            reducer = step,
                                            itemDesrialiser = idser,
                                            resultSerialiser = rser,    
                                            linNo = LineNo 1,
                                            accumulator = seed
                                          }
  in
    do
      hSrc <- safeOpenFile srcPth ReadMode
      eitherf hSrc
          (pure . Left . LogIOError "Openning Source File")
          (\hIn -> 
                  (
                    do
                      rsltFile <- destPthFunc srcPth
                      outHndle <- safeOpenFile rsltFile S.WriteMode 
                      eitherf outHndle
                        (pure . Left . LogIOError "Openning Output File")
                        (\hOut -> finally (processLines hIn hOut) (S.hClose hOut) $> Right rsltFile)        
                  ) 
                  `finally`
                      hClose hIn
          )
                  
prettyPrintLogprotocolReducer :: LineNo                         -- lineNo
          -> ()                                          -- accum
          -> Either DeserialisationError LogProtocol            -- Logprotocol
          -> ((), Maybe [Text])                     -- (newAccum, err / result)
prettyPrintLogprotocolReducer _ _ ethLp = ((), Just [either txtPretty (prettyPrintLogProtocol False) ethLp])