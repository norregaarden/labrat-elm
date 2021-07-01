module Pages.Data exposing (Model, Msg, page)

import Dict
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onDoubleClick)
import Element.Font as Font
import File.Download as Download
import Html
import Html.Attributes
import Husk
import Json.Encode as Encode
import Log
import Spil exposing (Spil(..))
import Storage exposing (Storage)
import TimeStr
import Element exposing (Element, alignBottom, alignLeft, alignRight, alpha, behindContent, centerX, column, el, fill, fillPortion, height, htmlAttribute, inFront, moveUp, padding, paddingEach, paddingXY, paragraph, px, rgba255, row, spaceEvenly, spacing, text, width)
import Gen.Params.Data exposing (Params)
import Page
import Request
import Shared
import Task
import Time
import UI exposing (appButton, bltr, p, s, small, smallAppButton)
import UIColor exposing (gray, greenToRed, orangeDark, red, scaleRatio, white)
import View exposing (View)


-- SETTINGS

barHeight
  = 70

barWidth
  = 30

dAlpha
  = 0.666

dAlpha2
  = 0.666/2

--


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
  Page.element
    { init = init shared
    , update = update shared.storage
    , view = view shared
    , subscriptions = \_ -> Sub.none
    }



-- INIT


type alias Model =
  { now : Time.Posix
  , zone : Time.Zone
  , labelKeys : List (Int, LogType)
  , baseline : Baseline
  , clicked : Int
  , popup : Popup
  }

type alias Baseline =
  { dut : Spil.Score_Dut
  , tid : Spil.Score_Tid
  , husk : Spil.Score_Husk
  }

type alias Popup =
  { popup : Bool
  , key : Int
  , label : LogType
  , delete : Bool
  }

type LogType
  = Log
  | Play


initBaseline =
  { dut =
    { mean = 1234
    , spread = 345 -- not used
    , correct = 10 -- not used
    , rounds = 10 -- not used
    }
  , tid =
    { burde = 10000 -- not used
    , faktisk = 10000 -- not used
    }
  , husk =
    { huskNumber = 6 -- not used
    , totalMistakes = 3 -- not used
    }
  }

initPopup =
  { popup = False
  , key = 0
  , label = Log
  , delete = False
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
    , clicked = 0
    , popup = initPopup
    }
  , Task.perform FindTime <| Task.map2 Tuple.pair Time.now Time.here
  )



-- UPDATE


type Msg
    = FindTime (Time.Posix, Time.Zone)
    | Click Int
    | EditClick (Int, LogType)
    | ClosePopup
    | Delete
    | ReallyDelete
    | DownloadData


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
  case msg of
    FindTime (now, zone) ->
      ( { model
        | now = now
        , zone = zone
        }
      , Cmd.none )

    Click key ->
      ( { model
        | clicked = key
        }
      , Cmd.none )

    EditClick (key, label) ->
      ( { model
        | popup =
          { popup = True
          , key = key
          , label = label
          , delete = False
          }
        }
      , Cmd.none )

    ClosePopup ->
      ( { model | popup = initPopup }
      , Cmd.none )

    Delete ->
      let
        popup = model.popup
        newPopup =
          { popup
          | delete = True
          }
      in
      ( { model | popup = newPopup }
      , Cmd.none )

    ReallyDelete ->
      if model.popup.popup && model.popup.delete then
        let
          del =
            case model.popup.label of
              Log ->
                Storage.deleteData
              Play ->
                Storage.deleteScores
        in
        ( { model | popup = initPopup }
        , del model.popup.key storage
        )
      else
        ( model, Cmd.none )

    DownloadData ->
      ( model
      , Encode.encode 2 (Storage.toJson storage)
        |> Download.string
          ("labratapp " ++ TimeStr.toDateTime model.zone model.now ++ ".json")
          "text/json"
      )




-- VIEW

viewPopup popup =
  let
    buttonAttrs =
      [ paddingXY (s 3) (s -1), Border.rounded 10, centerX ]
    deleteAttrs =
      onClick Delete ::
      buttonAttrs ++
      [ Font.color red, Background.color white ]
    sureAttrs =
      onClick ReallyDelete ::
      buttonAttrs ++
      [ Font.color white, Background.color red ]
    cancelAttrs =
      onClick ClosePopup ::
      buttonAttrs ++
      [ Font.color white, Background.color gray ]
    buttons =
      case popup.delete of
        False ->
          [ (text "Delete") |> el deleteAttrs
          , (text "Cancel") |> el cancelAttrs
          ]
        True ->
          [ (text "Cancel") |> el cancelAttrs
          , (text "Delete?") |> el sureAttrs
          ]
  in
  row
    [ width fill, height fill
    , Background.color orangeDark
    , spacing (s 3)
    ]
    buttons


viewTimeAndEdit : (Int, LogType) -> Time.Posix -> Time.Zone -> Int -> Element Msg
viewTimeAndEdit (key, label) time zone clicked =
  let
    left =
      time |> TimeStr.toDateTime zone |> text |> el [Font.size (s -1), alignLeft]

    right =
      if key == clicked then
        text "Edit" |> el
          [ Font.size (s -1), Font.underline
          , onClick (EditClick (key, label))
          , alignRight ]
      else
        Element.none
  in
  row
    [ width fill, paddingEach (bltr (s 1) 0 0 0) ]
    [ left, right ]


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
      dataColumn
        [ paragraph
            [ Html.Attributes.style "white-space" "pre-wrap"
                |> htmlAttribute
            ]
            [ Element.html (Html.text string) ]
        ]

    Log.DrugAdmin drug roa weight ->
      dataColumn
        [ dataRow "Drug" drug
        , dataRow "ROA" (Log.text_roa roa)
        , dataRow "Dose" (Log.text_weight weight)
        ]


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



viewData shared model =
  let
    popupAttribute key =
      if model.popup.popup && model.popup.key == key then
        inFront (viewPopup model.popup)
      else
        behindContent Element.none

    viewSingleData (key, label) =
      case label of
        Log ->
          case Dict.get key shared.storage.log of
            Nothing ->
              column []
                [ p <| "Deleted entry from "
                , p <| TimeStr.toDateTime model.zone (Time.millisToPosix key) ]

            Just value ->
              column [ width fill, onClick (Click key) ]
                [ viewTimeAndEdit (key, label) (Time.millisToPosix value.time) model.zone model.clicked
                , viewSingleLog value.data
                ]

        Play ->
          case Dict.get key shared.storage.playlog of
            Nothing ->
              column []
                [ p <| "Deleted entry from "
                , p <| TimeStr.toDateTime model.zone (Time.millisToPosix key) ]

            Just value ->
              column [ width fill, onClick (Click key), popupAttribute key ]
                [ viewTimeAndEdit (key, label) (Time.millisToPosix key) model.zone model.clicked
                , viewSinglePlay value model.baseline
                , viewSpilMeta
                ]

    accumulator (key, label) acc =
      el [paddingXY 0 (s 3), width fill, Border.widthEach (bltr 0 0 1 0), popupAttribute key]
        ( viewSingleData (key, label) )
      :: acc
  in
  column [width fill] <| List.foldl accumulator [] model.labelKeys


view : Shared.Model -> Model -> View Msg
view shared model =
  { title = "data | lab rat"
  , body =
    [ row [width fill, Font.size (s 1)]
      [ TimeStr.toFullDay model.zone model.now |> text |> el [alignLeft]
      , TimeStr.toFullTime model.zone model.now |> text |> el [alignRight]
      ]
    , smallAppButton DownloadData "Download data" |> el [centerX, padding (s 3)]
    , viewData shared model
    , text ""
    , text ""
    --, p "Note: Height of colored bars are not linear."
    ]
  }
