module Spil exposing (..)

import Element
import Gen.Route as Route
import UI


-- Bruges i Shared.elm

type Spil
    = Dut
    | Tid
    | Husk

alleSpil = [Dut, Tid, Husk] -- games

type Score
    = DutScore Score_Dut
    | TidScore Score_Tid
    | HuskScore Score_Husk

type alias Scores =
  { dut : Maybe Score_Dut
  , tid : Maybe Score_Tid
  , husk : Maybe Score_Husk
  }



-- Spillene

-- alle tider er millisekunder

type alias Score_Dut =
  { mean : Int
  , spread : Int
  , correct : Int
  , rounds : Int
  }

type alias Score_Tid =
  { burde : Int
  , faktisk : Int
  }

type alias Score_Husk =
  { huskNumber : Int
  , totalMistakes : Int
  }


-- Helpers

spilRoute spil =
  case spil of
    Dut -> Route.Spil__Dut
    Tid -> Route.Spil__Tid
    Husk -> Route.Spil__Husk

updateScores scores new =
  case new of
    DutScore score ->
      { scores | dut = Just score }
    TidScore score ->
      { scores | tid = Just score }
    HuskScore score ->
      { scores | husk = Just score }

updateGames games score =
  case score of
    DutScore _ ->
      List.filter (\g -> g /= Dut) games
    TidScore _ ->
      List.filter (\g -> g /= Tid) games
    HuskScore _ ->
      List.filter (\g -> g /= Husk) games

videreButton playing videreMsg okMsg =
  case playing of
    Nothing ->
      UI.appButton okMsg "OK"
      |> Element.el [Element.centerX, Element.padding (UI.s 2)]
    Just playingModel ->
      case playingModel.games of
        [s] ->
          UI.appButton videreMsg "Save all"
          |> Element.el [Element.centerX, Element.padding (UI.s 2)]
        list ->
          UI.appButton videreMsg "Next game"
          |> Element.el [Element.centerX, Element.padding (UI.s 2)]