{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.DocInterpreterUtils
  ( docErr,
    docErr2,
    docErr3,
    docErr4,
    docErrn,
    docFake,
    docFake2,
    docFake3,
    docFake4,
    docFaken,
    docAction,
    docAction2,
    docAction3,
    docAction4,
    docActionn,
    DocException (..),
  )
where

import DSL.Internal.NodeEvent (FrameworkLog (Step), NodeEvent (..))
import DSL.OutEffect (Out, out)
import Data.Text qualified as T
import Effectful as EF
  ( Eff,
    IOE,
    type (:>),
  )

-- import BasePrelude (throw)

data DocException
  = DocException Text
  | DocException' Text SomeException
  deriving (Show)

instance Exception DocException

{-
adaptException :: forall es a. (HasCallStack, IOE :> es{- , E.Error DocException :> es -}) => IO a -> Eff es a
adaptException m = EF.liftIO m `catch` \(e :: SomeException) -> E.throwError . DocException' "Exception thrown in documenter" $ e
-}

-- TODO:
-- sort out lazy IO
-- simple console effect
--   - add deferred validation
-- read file effect
--  - repro issue
--  - solve issue
-- finish doc file system
--   - demo simple efffect app including returning a doc value exception IO and step listing

-- TODO: implement docVal, docHush, docVoid, docVal', or docVoid'

logStep :: (Out NodeEvent :> es) => Text -> Eff es ()
logStep = out . Framework . Step

logDesc :: (HasCallStack, Out NodeEvent :> es) => [Text] -> Eff es ()
logDesc = logStep . T.intercalate " "

docErrn :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> [Text] -> Eff es a
docErrn funcName dscFrags =
  logDesc dscFrags
    >> ( pure . error $
           "\nException thrown in step documentation."
             <> "\n  Value forced from function: '"
             <> funcName
             <> "' in documentation mode."
             <> "\n  Use  docVal, docHush, docVoid, docVal'"
             <> " to replace or silence this value from where the step is called: '"
             <> funcName
             <> "'"
       )

-- TODO :: Swith to custom DocException exception but must return full callstack
-- doesn't do it now but might fix itself with GHC 9.10.00 ~ otherwise need to investigate
-- pure . throw . DocException $

docErr :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Eff es a
docErr funcName funcDesc = docErrn funcName [funcDesc]

docErr2 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Text -> Eff es a
docErr2 funcName funcDesc1 funcDesc2 = docErrn funcName [funcDesc1, funcDesc2]

docErr3 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Text -> Text -> Eff es a
docErr3 funcName funcDesc1 funcDesc2 funcDesc3 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3]

docErr4 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Text -> Text -> Text -> Eff es a
docErr4 funcName funcDesc1 funcDesc2 funcDesc3 funcDesc4 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3, funcDesc4]

docActionn :: forall es. (HasCallStack, IOE :> es, Out NodeEvent :> es) => [Text] -> Eff es ()
docActionn dscFrags = logStep $ T.intercalate " " dscFrags

docAction :: forall es. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Eff es ()
docAction funcDesc = docActionn [funcDesc]

docAction2 :: forall es. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Eff es ()
docAction2 funcDesc1 funcDesc2 = docActionn [funcDesc1, funcDesc2]

docAction3 :: forall es. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Text -> Eff es ()
docAction3 funcDesc1 funcDesc2 funcDesc3 = docActionn [funcDesc1, funcDesc2, funcDesc3]

docAction4 :: forall es. (HasCallStack, IOE :> es, Out NodeEvent :> es) => Text -> Text -> Text -> Text -> Eff es ()
docAction4 funcDesc1 funcDesc2 funcDesc3 funcDesc4 = docActionn [funcDesc1, funcDesc2, funcDesc3, funcDesc4]

docFaken :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => a -> [Text] -> Eff es a
docFaken a dscFrags = logDesc dscFrags >> pure a

docFake :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => a -> Text -> Eff es a
docFake a funcDesc = docFaken a [funcDesc]

docFake2 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => a -> Text -> Text -> Eff es a
docFake2 a funcDesc1 funcDesc2 = docFaken a [funcDesc1, funcDesc2]

docFake3 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => a -> Text -> Text -> Text -> Eff es a
docFake3 a funcDesc1 funcDesc2 funcDesc3 = docFaken a [funcDesc1, funcDesc2, funcDesc3]

docFake4 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es) => a -> Text -> Text -> Text -> Text -> Eff es a
docFake4 a funcDesc1 funcDesc2 funcDesc3 funcDesc4 = docFaken a [funcDesc1, funcDesc2, funcDesc3, funcDesc4]
