module Log exposing
  ( Data(..)
  , Drug
  , WeightUnit(..), allWUs
  , DosageQualifier(..), allDQs
  , Weight(..)
  , ROA, allROAs
  , text_roa, text_weight
  , text_wu, text_dq
  , roa_from, wu_from, dq_from
  , millisFromMinutes)

-- GENERAL

type Data
    = HR Int
    | TempC Int
    | BP Int Int
    | Musing String
    | DrugAdmin Drug ROA Weight



-- DRUG

type alias Drug = String


type WeightUnit
  = Microgram
  | Milligram

allWUs = [Microgram, Milligram]

type DosageQualifier
  = Threshold
  | Light
  | Common
  | Strong
  | Heavy

allDQs = [Threshold, Light, Common, Strong, Heavy]

type Weight
  = Quan WeightUnit Int
  | Qual DosageQualifier



type ROA
  = Insufflated
  | Intravenous
  | Oral
  | Rectal
  | Smoked
  | Sublingual

allROAs = [Insufflated, Intravenous, Oral, Rectal, Smoked, Sublingual]


-- HELPERS

text_roa roa =
  case roa of
    Insufflated -> "Insufflated"
    Intravenous -> "Intravenous"
    Oral -> "Oral"
    Rectal -> "Rectal"
    Smoked -> "Smoked"
    Sublingual -> "Sublingual"

roa_from text =
  case text of
    "Insufflated" -> Insufflated
    "Intravenous" -> Intravenous
    "Oral" -> Oral
    "Rectal" -> Rectal
    "Smoked" -> Smoked
    "Sublingual" -> Sublingual
    _ -> Sublingual

text_weight weight =
  case weight of
    Quan unit amount ->
      String.fromInt amount ++ " " ++ text_wu unit
    Qual qualifier ->
      text_dq qualifier


text_wu wu =
  case wu of
    Microgram -> "μg"
    Milligram -> "mg"

wu_from text =
  case text of
    "μg" -> Microgram
    "ug" -> Microgram
    "Microgram" -> Microgram
    "mg" -> Milligram
    "Milligram" -> Milligram
    _ -> Milligram

text_dq dq =
  case dq of
    Threshold -> "Threshold"
    Light -> "Light"
    Common -> "Common"
    Strong -> "Strong"
    Heavy -> "Heavy"

dq_from text =
  case text of
    "Threshold" -> Threshold
    "Light" -> Light
    "Common" -> Common
    "Strong" -> Strong
    "Heavy" -> Heavy
    _ -> Threshold


-- TIME

millisFromMinutes minutes =
  minutes * 60 * 1000