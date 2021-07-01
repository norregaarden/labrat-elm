module Pages.Data exposing (Model, Msg, page)

import Dict
import Element.Font as Font
import Log
import Spil
import Storage
import TimeStr
import Element exposing (Element, alignLeft, alignRight, column, el, fill, paddingXY, row, text, width)
import Gen.Params.Data exposing (Params)
import Page
import Request
import Shared
import Task
import Time
import UI exposing (p, s, small)
import View exposing (View)


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
  }


type LogType
  = Log
  | Play

logTypeText logType =
  case logType of
    Log -> "Log"
    Play -> "Play"


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
  small <| (++) "at : " <| TimeStr.toDateTime zone <| Time.millisToPosix timems

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
        , dataRow "Weight" (Log.text_weight weight)
        ]



viewSinglePlay : Spil.Scores -> Element msg
viewSinglePlay playEntry =
  text <| Debug.toString playEntry


viewData labelKeys logDict playDict zone =
  let
    viewTime key =
      small <| (++) "key: " <|
        TimeStr.toDateTime zone (Time.millisToPosix key)

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
              viewSinglePlay value

    accumulator (key, label) acc =
      column [paddingXY 0 (s 1), width fill]
        [ viewTime key
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
    , viewData model.labelKeys shared.storage.log shared.storage.playlog model.zone
    ]
  }
