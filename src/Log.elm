module Log exposing (Data(..), Drug, Weight(..))

type Data
  = HR Int
  | TempC Int
  | BP Int Int
  | Musing String
  | Intox Drug Weight

--allDataKinds = [HR, TempC, BP, Musing]

type alias Drug = String
{-
type Drug
  = Arylcyclohexylamine String
  | Phenetylamine String
  | Tryptamine String
-}

type Weight
  = Microgram Int
  | Milligram Int
  | Gram Int



-- HELPERS

----
-- tid er nu altid i millisekunder (siden 1970/01/01 UTC)
----

{-
timeString : Time.Posix -> String
timeString tid =
  String.fromInt <| floor <| (/) (1000 / 60) <| toFloat <| Time.posixToMillis tid

stringTime : String -> Time.Posix
stringTime tidstr =
  Time.millisToPosix <| (*) (60 * 1000) <| Maybe.withDefault -42 <| String.toInt tidstr
-}