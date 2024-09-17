{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.DocInterpreterUtils (
  docErr,
  docErr2,
  docErr3,
  docErr4,
  docErrn,
  docErrn'
) where

import DSL.Out ( out, Out )
import Effectful as EF (
  Eff,
  IOE,
  type (:>),
 )

import DSL.Internal.NodeEvent (NodeEvent (..), FrameworkLog (Step))
import Data.Text qualified as T

{-
data DocException
  = DocException Text
  | DocException' Text SomeException
  deriving (Show)

instance Exception DocException

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

docErrn :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Text -> [Text] -> Eff es a
docErrn funcName dscFrags =
  do
    let funcDesc = T.intercalate " " dscFrags
    logStep funcDesc
    -- TODO :: replace this later when have code to process call
    -- stack right now out of the box call handling looks better
    -- E.throwError . DocException $
    pure $ error ""
    {-
    pure . error $
      "\nException thrown in step documentation."
        <> "\n  Value forced from function: '"
        <> funcName
        <> "' in documentation mode."
        <> "\n  Use  docVal, docHush, docVoid, docVal'"
        <> " to replace or silence this value from where the step is called: '"
        <> funcName
        <> "'"
        -}

docErrn' :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Text -> [Text] -> a -> Eff es a
docErrn' funcName dscFrags mockValue =
  do
    let funcDesc = T.intercalate " " dscFrags
    logStep funcDesc
    pure mockValue
   

docErr :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Text -> Text -> Eff es a
docErr funcName funcDesc = docErrn funcName [funcDesc]

docErr2 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Text -> Text -> Text -> Eff es a
docErr2 funcName funcDesc1 funcDesc2 = docErrn funcName [funcDesc1, funcDesc2]

docErr3 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Text -> Text -> Text -> Text -> Eff es a
docErr3 funcName funcDesc1 funcDesc2 funcDesc3 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3]

docErr4 :: forall es a. (HasCallStack, IOE :> es, Out NodeEvent :> es{- , E.Error DocException :> es -}) => Text -> Text -> Text -> Text -> Text -> Eff es a
docErr4 funcName funcDesc1 funcDesc2 funcDesc3 funcDesc4 = docErrn funcName [funcDesc1, funcDesc2, funcDesc3, funcDesc4]
