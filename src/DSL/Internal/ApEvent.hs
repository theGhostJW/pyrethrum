module DSL.Internal.ApEvent where

-- TODO: make log effect requiring out
data ApEvent =
    StartFolder Text
  | EndFolder
  | Log Text
  | Log' {
    message :: Text,
    details :: Text
  }
  | Warning Text
  | Warning' {
    message :: Text,
    details :: Text
  }
  | Error Text
  | Error' {
    message :: Text,
    details :: Text
  }
  | Step Text
  | Step' {
    message :: Text,
    details :: Text
  }
  deriving stock (Eq, Show)