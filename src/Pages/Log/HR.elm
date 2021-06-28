module Pages.Log.HR exposing (Model, Msg, page)

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
  { input : Int
  , validated : Bool
  }

init : ( Model, Cmd Msg )
init =
  ( Model 70 True
  , Cmd.none
  )



-- UPDATE


type Msg
    = ChangedInput Int
    | SavedInput
    | LogDataTid Time.Posix


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
  case msg of
    ChangedInput input ->
      ( { model | input = input, validated = isValid input }, Cmd.none )
    SavedInput ->
      ( model, Task.perform LogDataTid Time.now )
    LogDataTid tid ->
      ( model, Storage.logData (Time.posixToMillis tid) (Time.posixToMillis tid, HR model.input) storage )



isValid input =
  if input >= 30 && input <= 300 then
    True
  else
    False



-- VIEW

intForm model =
  let
    submitButton =
      case model.validated of
        True -> smallAppButton SavedInput
        False -> smallAppButtonDisabled
    validationText =
      case model.validated of
        True -> ""
        False -> "Enter a value between 30 and 300."
  in
  h 1 "Heart rate"
  :: row [spacing (s 2)]
    [ Input.text [Border.color orangeLight, Font.size (s 3)]
      { label = Input.labelAbove [] (text "beats per minute" |> el [Font.size (s 1)])
      , onChange = \unsafeInput -> Maybe.withDefault 0 (String.toInt unsafeInput) |> ChangedInput
      , placeholder = Just (Input.placeholder [] (text "bpm"))
      , text = model.input |> String.fromInt
      }
    , submitButton "log" |> el [alignBottom]
    ]
  :: [UI.p validationText |> el [Font.size (s 1)]]
  |> column [spacing (s 2)]

view : Storage -> Model -> View Msg
view _ model =
  let
    body =
      [ intForm model |> el [Font.size (s 2)]
      ]
  in
  { title = "log | lab rat"
  , body = body
  }
