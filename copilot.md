```haskell
chkErrorPropagation :: [ExeEvent] -> IO ()
chkErrorPropagation evts =
  traverse_ reconcileParents $ threadIds evts
  where
    reconcileParents :: SThreadId -> IO ()
    reconcileParents tid =
      let cpMap' = cpMap tid
          failMap = fails tid
          etMap = evtTypeMap tid
          -- get the parent or if it is a grouping event
          -- get it's paraent Grouping events (Groups and Fixtures) are effectively
          -- ignored in the error propagation
          truParent :: Loc -> Loc
          truParent parentLoc =
            let nxtParent = truParent $ lookupThrow "parent not found in child parent map" cpMap' parentLoc
                parentIsGrouping = isGrouping $ lookupThrow "loc not found in event map" etMap parentLoc
             in (parentLoc == Root)
                  ? Root
                  $ (parentIsGrouping ? nxtParent $ parentLoc)
          trueParentFailure pLoc = failMap M.!? truParent pLoc
       in traverse_
            ( \(chldLoc, pLoc) ->
              -- following 2 lines were cp
                isGrouping (lookupThrow "loc not found in event map" etMap chldLoc)
                  ? pure ()
                  $ failMap M.!? chldLoc
                    & maybe
                      ( -- the child event passed so parent must have passed
                        trueParentFailure pLoc
                          & maybe
                            (pure ())
                            ( error $
                                "Child event passed when parent failed - error should have propagated\nchild\n"
                                  <> ppShow chldLoc
                                  <> "\nparent\n"
                                  <> ppShow (truParent pLoc)
                            )
                      )
                      ( -- the child event failed
                        \case
                          Fail {floc = childLoc, exception = childExcption} ->
                            trueParentFailure pLoc
                              & maybe
                                (pure ())
                                (error $ "Child event failed (not propagated parent failure) when parent failed - parent error should have propagated\n" <> ppShow childLoc)
                          ParentFail {floc = childloc, ploc, exception = childException} ->
                            trueParentFailure pLoc
                              & maybe
                                (error $ "Child event has propagated parent failure when parent has not failed\n" <> ppShow childloc)
                                ( \p ->
                                    let pexcpt = SuiteRuntimeTest.exception p
                                     in chkEq'
                                          ( toS $
                                              "Propagated excption does not equal parent exception for loc:\n"
                                                <> ppShow childloc
                                                <> "\nchild exception\n"
                                                <> ppShow childException
                                                <> "\nparent exception\n"
                                                <> ppShow pexcpt
                                          )
                                          childException
                                          pexcpt
                                )
                      )
            )
            (M.toList cpMap')

    thrdIds = threadIds evts
    evtTypeMap thrdId =
      foldl'
        ( \acc ->
            \case
              Start eet loc _n sti -> eventBelongsToThread thrdId sti eet ? M.insert loc eet acc $ acc
              _ -> acc
        )
        M.empty
        evts
    cpMap = actualChildParentMap evts

    ```