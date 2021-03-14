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
import Polysemy.Error as PE
import qualified Data.DList as D
import RunElementClasses as C
import OrphanedInstances()
import Data.Aeson
import RunnerBase as RB
import qualified Data.Foldable as F

runItem :: forall e effs rc tc i as ds. (MinEffs e effs, 
                                            Show e, 
                                            ItemClass i ds, 
                                            TestConfigClass tc, 
                                            ToJSON e, 
                                            ToJSON as, 
                                            ToJSON ds) => ItemRunner e as ds i tc rc effs
runItem rc (Test tc _items interactor prepState) i  = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) (identifier i)

    logChk :: CK.CheckReport -> Sem effs ()
    logChk cr = logRP $ CheckOutcome iid cr

    recordSkippedChecks :: Sem effs ()
    recordSkippedChecks = do 
                            logRP StartChecks 
                            F.traverse_ logChk $ D.toList $ CK.skipChecks (checkList i)

    prepStateErrorHandler :: FrameworkError e -> Sem effs ds
    prepStateErrorHandler e = 
      do 
        logRP $ PrepStateFailure iid e
        recordSkippedChecks
        PE.throw e

    -- provided natively by polysemy in later versions of polysemy
    try' :: Member (Error er) r => Sem r a -> Sem r (Either er a)
    try' m = PE.catch (Right <$> m) (return . Left)
       
    runItem' :: Sem effs ()
    runItem' = 
      let
        runChecks :: ds -> Sem effs ()
        runChecks ds = F.traverse_ logChk $ D.toList $ CK.calcChecks ds (checkList i)
      in 
        do 
          logRP StartInteraction
          -- TODO: check for io exceptions / SomeException - use throw from test
          log "interact start"
          ethApState <- try' $ interactor rc i
          eitherf ethApState
            (\e -> do 
                    logRP $ InteractorFailure iid e
                    logRP $ PrepStateSkipped iid
                    recordSkippedChecks
                    )
            (\as -> do 
                log "interact end"
                logRP . InteractorSuccess iid . ApStateJSON . toJSON $ as
                logRP StartPrepState
                ds <- PE.catch (prepState i as) prepStateErrorHandler
                logRP . PrepStateSuccess iid . DStateJSON . toJSON $ ds
                logRP StartChecks
                runChecks ds
              )
  in 
    runItem' `PE.catch` (logRP . LP.Error)

documentItem :: forall e effs rc tc i as ds. (ToJSON e, 
                                                Show e, 
                                                ItemClass i ds,
                                                 TestConfigClass tc, 
                                                 Member (Logger e) effs)
                                                => ItemRunner e as ds i tc rc effs
documentItem rc (Test tc _items interactor _prepState) i = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) $ identifier i

    docLog :: DocProtocol e -> Sem effs ()
    docLog = logItem . logDoc

    logChecks :: Sem effs ()
    logChecks =  P.sequence_ $  
                    (\chk -> docLog $ DocCheck iid (CK.header (chk :: CK.Check ds)) (CK.expectation chk) (CK.gateStatus chk)) <$> D.toList (checkList i)
  in 
    do 
      docLog DocInteraction
      interactor rc i
      docLog DocChecks
      logChecks