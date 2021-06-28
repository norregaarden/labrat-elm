module Log exposing (Data(..), Drug, WeightUnit(..), allWUs, DosageQualifier(..), allDQs, Weight(..), ROA, allROAs, text_roa, text_wu, text_dq)

-- GENERAL

type Data
    = HR Int
    | TempC Int
    | BP Int Int
    | Musing String
    --| Intox Drug Weight



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

text_wu wu =
  case wu of
    Microgram -> "μg"
    Milligram -> "mg"

text_dq dq =
  case dq of
    Threshold -> "Threshold"
    Light -> "Light"
    Common -> "Common"
    Strong -> "Strong"
    Heavy -> "Heavy"
