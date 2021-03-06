module AuxFilesTest where

import Pyrelude
import Pyrelude.Test
import AuxFiles

unit_a_file_prefix_generated_at_a_later_date_will_be_smaller =
  let 
    timeEarly = TimeOfDay {timeOfDayHour = 0, timeOfDayMinute = 0, timeOfDayNanoseconds = 0}
    timeLate = TimeOfDay {timeOfDayHour = 0, timeOfDayMinute = 0, timeOfDayNanoseconds = 1000000}
    date = Date (Year 2000) january $ DayOfMonth 1
    pfxLate = logFilePrefix . datetimeToTime $ Datetime date timeLate
    pfxEarly = logFilePrefix . datetimeToTime $ Datetime date timeEarly
  in
    chk $ pfxLate < pfxEarly
