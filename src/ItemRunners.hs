{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module ItemRunners where

import qualified Check as CK
import Common as C
import DSL.Interpreter
import DSL.Logger
import DSL.LogProtocol as LP
import Pyrelude as P
import Polysemy
import Polysemy.Error as PE ( Error, catch, throw )
import qualified Data.DList as D
import RunElementClasses as C
    ( ItemClass(checkList, identifier), TestConfigClass(..) )
import OrphanedInstances()
import Data.Aeson ( ToJSON(toJSON) )
import RunnerBase as RB ( Test(Test), ItemRunner )
import qualified Data.Foldable as F

runItem :: forall e effs rc tc hi i as ds. (MinEffs e effs, 
                                            Show e, 
                                            ItemClass i ds, 
                                            TestConfigClass tc, 
                                            ToJSON e, 
                                            ToJSON as, 
                                            ToJSON ds) => ItemRunner e as ds i hi tc rc effs
runItem rc hi (Test tc _items interactor parse) i  = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) (identifier @i @ds i)

    logChk :: CK.CheckReport -> Sem effs ()
    logChk cr = logItem $ CheckOutcome iid cr

    recordSkippedChecks :: Sem effs ()
    recordSkippedChecks = do 
                            logItem StartChecks 
                            F.traverse_ logChk $ D.toList $ CK.skipChecks (checkList @i @ds i)

    parseErrorHandler :: FrameworkError e -> Sem effs ds
    parseErrorHandler e = 
      do 
        logItem $ ParserFailure iid e
        recordSkippedChecks
        PE.throw e

    -- provided natively by polysemy in later versions of polysemy
    try' :: Member (Error er) r => Sem r o -> Sem r (Either er o)
    try' m = PE.catch (Right <$> m) (return . Left)
       
    runItem' :: Sem effs ()
    runItem' = 
      let
        runChecks :: ds -> Sem effs ()
        runChecks ds = F.traverse_ logChk $ D.toList $ CK.calcChecks ds (checkList i)
      in 
        do 
          logItem StartInteraction
          -- TODO: check for io exceptions / SomeException - use throw from test
          log "interact start"
          ethApState <- try' $ interactor rc hi i
          eitherf ethApState
            (\e -> do 
                    logItem $ InteractorFailure iid e
                    logItem $ ParserSkipped iid
                    recordSkippedChecks
                    )
            (\as -> do 
                log "interact end"
                logItem . InteractorSuccess iid . ApStateJSON . toJSON $ as
                logItem StartParser
                ds <- PE.catch (parse as) parseErrorHandler
                logItem . ParserSuccess iid . DStateJSON . toJSON $ ds
                logItem StartChecks
                runChecks ds
              )
  in 
    runItem' `PE.catch` (logItem . LP.Error)

documentItem :: forall e effs rc tc hi i as ds. (ToJSON e, 
                                                Show e, 
                                                ItemClass i ds,
                                                 TestConfigClass tc, 
                                                 Member (Logger e) effs)
                                                => ItemRunner e as ds i hi tc rc effs
documentItem rc hi (Test tc _items interactor _parse) i = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) $ identifier @i @ds i

    lgChk :: CK.Check ds -> Sem effs ()
    lgChk chk = logItem . CheckOutcome iid 
                                      . CK.CheckReport CK.Skip 
                                      $ DetailedInfo (CK.header (chk :: CK.Check ds) <> " - " <> txt (CK.expectation chk)) ""

    logChecks :: Sem effs ()
    logChecks =  P.sequence_ $ lgChk <$> D.toList (checkList i)
  in 
    do 
      logItem StartInteraction 
      interactor rc hi i
      logItem StartChecks
      logChecks