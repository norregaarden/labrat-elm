module Pages.Data exposing (Model, Msg, page)

import Dict
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Husk
import Log
import Spil exposing (Spil(..))
import Storage
import TimeStr
import Element exposing (Element, alignBottom, alignLeft, alignRight, alpha, centerX, column, el, explain, fill, fillPortion, height, inFront, moveDown, moveUp, padding, paddingEach, paddingXY, px, rgba255, row, spaceEvenly, spacing, text, width)
import Gen.Params.Data exposing (Params)
import Page
import Request
import Shared
import Task
import Time
import UI exposing (bltr, p, s, small)
import UIColor exposing (gray, green, greenToRed, scaleRatio)
import View exposing (View)


-- SETTINGS

barHeight
  = 70

barWidth
  = 20

dAlpha
  = 0.666

dAlpha2
  = 0.666/2

--



page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
  Page.element
    { init = init shared
    , update = update
    , view = view shared
    , subscriptions = \_ -> Sub.none
    }



-- INIT


type alias Model =
  { now : Time.Posix
  , zone : Time.Zone
  , labelKeys : List (Int, LogType)
  , baseline : Baseline
  }

type alias Baseline =
  { dut : Spil.Score_Dut
  , tid : Spil.Score_Tid
  , husk : Spil.Score_Husk
  }


type LogType
  = Log
  | Play

logTypeText logType =
  case logType of
    Log -> "Log"
    Play -> "Play"


initBaseline =
  { dut =
    { mean = 1234
    , spread = 345 -- not used
    , correct = 10 -- not used
    , rounds = 10 -- not used
    }
  , tid =
    { burde = 10000
    , faktisk = 10000
    }
  , husk =
    { huskNumber = 6
    , totalMistakes = 3
    }
  }

init : Shared.Model -> ( Model, Cmd Msg )
init shared =
  let
    labelKeys =
      List.map
        (\k -> (k, Log))
        (Dict.keys shared.storage.log)
      ++
      List.map
        (\k -> (k, Play))
        (Dict.keys shared.storage.playlog)
  in
  ( { now = Time.millisToPosix 0
    , zone = Time.utc
    , labelKeys = List.sortBy Tuple.first labelKeys
    , baseline = initBaseline
    }
  , Task.perform FindTime <| Task.map2 Tuple.pair Time.now Time.here
  )



-- UPDATE


type Msg
    = FindTime (Time.Posix, Time.Zone)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FindTime (now, zone) ->
      ( { model
        | now = now
        , zone = zone
        }
      , Cmd.none )



-- VIEW


viewSingleLogTime : Int -> Time.Zone -> Element msg
viewSingleLogTime timems zone =
  el [Font.size (s -1), paddingEach (bltr (s 1) 0 0 0)] <| text <| (++) "" <| TimeStr.toDateTime zone <| Time.millisToPosix timems


viewSingleLog : Log.Data -> Element msg
viewSingleLog log =
  let
    dataRow left right =
      row [width fill]
        [ left |> text |> el [alignLeft]
        , right |> text |> el [alignRight]
        ]

    dataColumn rows =
      column [width fill] rows
  in
  case log of
    Log.HR bpm ->
      dataRow "Heart rate" (String.fromInt bpm ++ " bpm")

    Log.TempC tc ->
      dataRow "Temperature" (String.fromInt tc ++ " °C")

    Log.BP high low ->
      dataRow "Blood Pressure" (String.fromInt high ++ " / " ++ String.fromInt low)

    Log.Musing string ->
      dataColumn [text "Musing: ", p string]

    Log.DrugAdmin drug roa weight ->
      dataColumn
        [ dataRow "Drug" drug
        , dataRow "ROA" (Log.text_roa roa)
        , dataRow "Dose" (Log.text_weight weight)
        ]


viewDebugPlay : Spil.Scores -> Element msg
viewDebugPlay playEntry =
  let
    dutView =
      case playEntry.dut of
        Nothing ->
          text "næ"
        Just dutScore ->
          small <| Debug.toString dutScore

    tidView =
      case playEntry.tid of
        Nothing ->
          text "næ"
        Just tidScore ->
          small <| Debug.toString tidScore

    huskView =
      case playEntry.husk of
        Nothing ->
          text "næ"
        Just huskScore ->
          small <| Debug.toString huskScore

  in
  row [width fill] [dutView, tidView, huskView]


viewSinglePlay : Spil.Scores -> Baseline -> Element msg
viewSinglePlay playEntry baseline =
  let
    bar attrs = (Element.none) |> el attrs

    intRatio top bot =
      (toFloat top) / (toFloat bot)

    intRatioScale top bot =
      intRatio top bot
      |> scaleRatio

    intRatioScaleHeight top bot =
      intRatioScale top bot
      |> (*) barHeight

    floatToHeight float =
      float
      |> round |> px |> height

    rowAttrs =
      [width (fillPortion 1), paddingXY (s 2) (s 4), spacing (s 1), alignBottom]

    barColAttrs =
      [barWidth |> px |> width, alignBottom, centerX]

    dutView =
      case playEntry.dut of
        Nothing ->
          text "næ"
        Just dutScore ->
          let
            spreadHeight =
              intRatioScaleHeight dutScore.spread baseline.dut.mean
            meanScale =
              intRatioScale dutScore.mean baseline.dut.mean
            correctRatio =
              (intRatio dutScore.correct dutScore.rounds)
          in
          row rowAttrs
            [ column barColAttrs
              [ bar
                [ meanScale |> (*) barHeight |> floatToHeight
                , Background.color (greenToRed (1/meanScale))
                , width fill
                , inFront <| bar
                  [ spreadHeight |> floatToHeight
                  , moveUp (0.5 * spreadHeight)
                  , Background.color gray
                  , width fill
                  , alpha dAlpha2
                  ]
                ]
              ]
            , column barColAttrs
              <| List.repeat (dutScore.rounds - dutScore.correct)
                (bar
                  [ width fill
                  , floatToHeight (intRatio barHeight dutScore.rounds)
                  , Background.color (greenToRed (correctRatio*0.5))
                  , Border.widthEach (bltr 0 0 1 0)
                  , Border.color (rgba255 0 0 0 dAlpha2)
                  ]
                )
              ++ List.repeat dutScore.correct
                (bar
                  [ width fill
                  , floatToHeight (intRatio barHeight dutScore.rounds)
                  , Background.color (greenToRed (correctRatio*2))
                  , Border.widthEach (bltr 0 0 1 0)
                  , Border.color (rgba255 0 0 0 dAlpha2)
                  ]
                )
            ]

    tidView =
      case playEntry.tid of
        Nothing ->
          text "næ"
        Just tidScore ->
          let
            tidBarHeight =
              barHeight * dAlpha
            tidScale =
              intRatioScale (tidScore.faktisk) (tidScore.burde)
            tidHeight =
              tidScale * tidBarHeight
          in
          row [width (fillPortion 1), paddingXY (s 2) (s 4), spacing (s 1), alignBottom]
            [ column [barWidth |> px |> width, alignBottom, centerX]
              [ bar
                [ tidHeight |> floatToHeight
                , min (tidScale) (1/tidScale) |> greenToRed |> Background.color
                , width fill
                , inFront <| bar
                  [ tidBarHeight |> floatToHeight
                  , moveUp (tidBarHeight - tidHeight)
                  , Border.widthEach (bltr 0 0 1 0)
                  , Border.color (rgba255 0 0 0 dAlpha)
                  , width fill
                  ]
                ]
              ]
            ]

    huskView =
      case playEntry.husk of
        Nothing ->
          text "næ"
        Just huskScore ->
          let
            totalHuskere =
              List.length Husk.allImages
            huskRatio =
              (intRatio huskScore.huskNumber totalHuskere)
          in
          row rowAttrs
            [ column barColAttrs
              <| List.repeat (totalHuskere - huskScore.huskNumber)
                (bar
                  [ width fill
                  , floatToHeight (intRatio barHeight totalHuskere)
                  , Background.color (greenToRed (huskRatio*0.5))
                  , Border.widthEach (bltr 0 0 1 0)
                  , Border.color (rgba255 0 0 0 dAlpha2)
                  ]
                )
              ++ List.repeat huskScore.huskNumber
                (bar
                  [ width fill
                  , floatToHeight (intRatio barHeight totalHuskere)
                  , Background.color (greenToRed (huskRatio*2))
                  , Border.widthEach (bltr 0 0 1 0)
                  , Border.color (rgba255 0 0 0 dAlpha2)
                  ]
                )
            , column barColAttrs
              <| List.repeat huskScore.totalMistakes
                (bar
                  [ width fill
                  , floatToHeight (intRatio barHeight totalHuskere)
                  , Background.color
                    (greenToRed (intRatio totalHuskere huskScore.totalMistakes))
                  , Border.widthEach (bltr 0 0 1 0)
                  , Border.color (rgba255 0 0 0 dAlpha2)
                  ]
                )
            ]


  in
  row [width fill] [dutView, tidView, huskView]


viewSpilMeta : Element msg
viewSpilMeta =
  let
    accumulator spil acc =
      let
        tekst =
          case spil of
            Dut -> "Visual search"
            Tid -> "10 seconds"
            Husk -> "Short-term memory"
      in
      el [width (fillPortion 1), Font.size (s -3), Font.center] (text tekst)
      :: acc
  in
  row [width fill, spacing (s 1)] <| List.foldr accumulator [] Spil.alleSpil




viewData labelKeys logDict playDict baseline zone =
  let
    viewTime (key, label) =
      case label of
        Play ->
          el [Font.size (s -1)] <| text <| (++) "" <|
            TimeStr.toDateTime zone (Time.millisToPosix key)
        Log ->
          Element.none

    viewSingleData (key, label) =
      case label of
        Log ->
          case Dict.get key logDict of
            Nothing ->
              text <| "ERROR key "
                ++ String.fromInt key
                ++ " not found in log"

            Just value ->
              column [width fill]
                [ viewSingleLogTime value.time zone
                , viewSingleLog value.data
                ]

        Play ->
          case Dict.get key playDict of
            Nothing ->
              text <| "ERROR key "
                ++ String.fromInt key
                ++ " not found in playlog"

            Just value ->
              column [width fill]
                [ -- viewDebugPlay value
                viewSinglePlay value baseline
                , viewSpilMeta
                ]

    accumulator (key, label) acc =
      column [paddingXY 0 (s 3), width fill, Border.widthEach (bltr 0 0 1 0)]
        [ viewTime (key, label)
        , viewSingleData (key, label)
        ]
      :: acc
  in
  column [width fill] <| List.foldl accumulator [] labelKeys


view : Shared.Model -> Model -> View Msg
view shared model =
  { title = "data | lab rat"
  , body =
    [ row [width fill, Font.size (s 1)]
      [ TimeStr.toFullDay model.zone model.now |> text |> el [alignLeft]
      , TimeStr.toFullTime model.zone model.now |> text |> el [alignRight]
      ]
    --, p (Debug.toString shared.playing)
    --, text "shared.storage"
    --, p (Debug.toString shared.storage.playlog)
    , viewData model.labelKeys shared.storage.log shared.storage.playlog model.baseline model.zone
    , text ""
    , p "Note: Height of colored bars are not linear."
    ]
  }
