module Spil exposing (..)

import Gen.Route as Route


-- Bruges i Shared.elm

type Spil
  = Dut
  | Tid

alleSpil = [Dut, Tid] -- games

type Score
  = DutScore Score_Dut
  | TidScore Score_Tid

type alias Scores =
  { dut : Maybe Score_Dut
  , tid : Maybe Score_Tid
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



-- Helpers

spilRoute spil =
  case spil of
    Dut -> Route.Spil__Dut
    Tid -> Route.Spil__Tid

updateScores scores new =
  case new of
    DutScore score ->
      { scores | dut = Just score }
    TidScore score ->
      { scores | tid = Just score }

updateGames games score =
  case score of
    DutScore _ ->
      List.filter (\g -> g /= Dut) games
    TidScore _ ->
      List.filter (\g -> g /= Tid) games
