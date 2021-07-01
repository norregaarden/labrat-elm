module TimeStr exposing (..)

import Time exposing (Month(..), Weekday(..), Zone)


toDateTime zone time =
  toYear zone time
  ++ "-"
  ++ toMonthNumber zone time
  ++ "-"
  ++ toDay zone time
  ++ " "
  ++ toFullTime zone time


toFullDay zone time =
  ""
  ++ toWeekday zone time
  ++ " "
  ++ toYear zone time
  ++ " "
  ++ toMonth zone time
  ++ " "
  ++ toDay zone time
  ++ ". "


toFullTime zone time =
  ""
  ++ toHour zone time
  ++ ":"
  ++ toMinute zone time
  ++ ":"
  ++ toSecond zone time


toWeekday zone time =
  case Time.toWeekday zone time of
    Mon -> "Mon"
    Tue -> "Tue"
    Wed -> "Wed"
    Thu -> "Thu"
    Fri -> "Fri"
    Sat -> "Sat"
    Sun -> "Sun"


toYear : Zone -> Time.Posix -> String
toYear zone time =
  String.fromInt (Time.toYear zone time)


toMonthNumber zone time =
  case Time.toMonth zone time of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

toMonth : Zone -> Time.Posix -> String
toMonth zone time =
  case Time.toMonth zone time of
    Jan -> "jan"
    Feb -> "feb"
    Mar -> "mar"
    Apr -> "apr"
    May -> "may"
    Jun -> "jun"
    Jul -> "jul"
    Aug -> "aug"
    Sep -> "sep"
    Oct -> "okt"
    Nov -> "nov"
    Dec -> "dec"


toDay : Zone -> Time.Posix -> String
toDay zone time =
  let
    day = String.fromInt (Time.toDay zone time)
  in
  if String.length day == 1 then
    "0" ++ day
  else
    day


toHour zone time =
  let
    hour = String.fromInt (Time.toHour zone time)
  in
  if String.length hour == 1 then
    "0" ++ hour
  else
    hour



toMinute zone time =
  let
    minute = String.fromInt (Time.toMinute zone time)
  in
  if String.length minute == 1 then
    "0" ++ minute
  else
    minute



toSecond zone time =
  let
    second = String.fromInt (Time.toSecond zone time)
  in
  if String.length second == 1 then
    "0" ++ second
  else
    second


