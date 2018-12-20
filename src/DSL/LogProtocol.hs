
module DSL.LogProtocol where

import           DSL.Common
import           Foundation.Extended

data LogProtocol rc tc =
                   Message String |
                   Message' DetailedInfo |

                   Warning String |
                   Warning' DetailedInfo |

                   Error AppError |
                   FilterLog [Either (FilterRejection tc) tc] |

                   StartRun rc |
                   StartGroup String |
                   StartTest tc |
                   StartIteration {
                               test :: String,
                               iid  :: Int
                             } |
                   EndIteration String


                  deriving (Eq, Show)
