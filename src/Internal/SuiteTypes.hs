module SuiteTypes where

-- fthis is a simplified demo only and can be deleted

data TestRunElement m oi oo ti to where
   OnceHook ::
    { title :: Text,
      ohook :: oi -> m oo,
      subElms :: [TestRunElement m oo oo' ti to]
    } ->
    TestRunElement m oi oo ii io
   ThreadHook ::
    { title :: Text,
      thook :: oi -> ti -> m to,
      subElms' :: TestRunElement m oi oo to to'
    } ->
    TestRunElement m oi oo ii io
   Fixtures ::
    { title :: Text,
      fHook :: Maybe (oi -> ti -> m ii),
      fixtures :: [Fixture m oi ti ii ]
    } ->
    TestRunElement m oi oo ii io

data Fixture m oi ti ii = Fixture {
  title :: Text,
  tests :: oi -> ti -> ii -> m ()
}