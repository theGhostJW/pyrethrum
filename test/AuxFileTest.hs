module AuxFileTest where

import AuxFiles
import Pyrelude as P
import Pyrelude.Test as T
import Data.Aeson


success' :: Result a -> a 
success' = \case
            Success a -> a
            _ -> error "Failed"

unit_a_file_prefix_generated_at_a_later_date_will_be_smaller = 
  let 
    pfxLate = logFilePrefix  . success' $ fromJSON "2020-05-02T20:18:39.659+1000"
    pfxEarly = logFilePrefix . success' $ fromJSON "2020-05-02T20:18:39.658+1000"
  in
   chk $ pfxLate < pfxEarly



   
