module Log exposing (Data(..), Drug, Weight(..))

type Data
    = HR Int
    | TempC Int
    | BP Int Int
    | Musing String
    | Intox Drug Weight


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