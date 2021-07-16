module Pages.Log.BP exposing (Model, Msg, page)

import Element exposing (alignBottom, column, el, row, spacing, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import Task
import Time
import UI exposing (h, s, smallAppButton, smallAppButtonDisabled)
import UIColor exposing (orangeLight)
import View exposing (View)
import Log exposing (Data(..))


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
  Page.element
    { init = init
    , update = update shared.storage
    , view = view shared.storage
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Model =
  { high : Int
  , low : Int
  }

init : ( Model, Cmd Msg )
init =
  ( Model 120 80
  , Cmd.none
  )



-- UPDATE


type Msg
    = ChangedHigh Int
    | ChangedLow Int
    | SavedInput
    | LogDataTid Time.Posix


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
  case msg of
    ChangedHigh high ->
      ( { model | high = high }, Cmd.none )
    ChangedLow low ->
      ( { model | low = low }, Cmd.none )
    SavedInput ->
      ( model, Task.perform LogDataTid Time.now )
    LogDataTid tid ->
      ( model, Storage.logData (Time.posixToMillis tid) (Time.posixToMillis tid, BP model.high model.low) storage )



-- VIEW

intIntForm model =
  let
    submitButton =
      smallAppButton SavedInput

    validationText =
      ""
  in
  h 1 "Blood Pressure"
  :: row [spacing (s 2)]
    [ Input.text [Border.color orangeLight, Font.size (s 3)]
      { label = Input.labelAbove [] (text "high" |> el [Font.size (s 1)])
      , onChange = \unsafeInput -> Maybe.withDefault 0 (String.toInt unsafeInput) |> ChangedHigh
      , placeholder = Just (Input.placeholder [] (text "mmHg"))
      , text = model.high |> String.fromInt
      }
    , Input.text [Border.color orangeLight, Font.size (s 3)]
        { label = Input.labelAbove [] (text "low" |> el [Font.size (s 1)])
        , onChange = \unsafeInput -> Maybe.withDefault 0 (String.toInt unsafeInput) |> ChangedLow
        , placeholder = Just (Input.placeholder [] (text "mmHg"))
        , text = model.low |> String.fromInt
        }
    , submitButton "log" |> el [alignBottom]
    ]
  :: [UI.p validationText |> el [Font.size (s 1)]]
  |> column [spacing (s 2)]

view : Storage -> Model -> View Msg
view _ model =
  let
    body =
      [ intIntForm model |> el [Font.size (s 2)]
      ]
  in
  { title = "log: blood pressure | lab rat"
  , body = body
  }
