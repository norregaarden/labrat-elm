module Pages.Log.TempC exposing (Model, Msg, page)

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
import Round


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
  { input : Float
  , inputString : String
  , validatedRange : Bool
  , validatedNumber : Bool
  }

init : ( Model, Cmd Msg )
init =
  ( Model 37.0 "37.0" True True
  , Cmd.none
  )



-- UPDATE


type Msg
    = ChangedInput String
    | SavedInput
    | LogDataTid Time.Posix


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
  case msg of
    ChangedInput input ->
      ( { model | inputString = input
        , input = fixInput input
        , validatedRange = isValidRange (Maybe.withDefault 0 (String.toFloat input))
        , validatedNumber = isValidNumber input
        }
      , Cmd.none )
    SavedInput ->
      ( model, Task.perform LogDataTid Time.now )
    LogDataTid tid ->
      ( model, Storage.logData (Time.posixToMillis tid) (Time.posixToMillis tid, TempC model.input) storage )


fixInput input =
  input |> String.toFloat |> Maybe.withDefault 0 
  |> Round.round 1 |> String.toFloat |> Maybe.withDefault 0


isValidRange input =
  if input >= 30 && input <= 45 then
    True
  else
    False


isValidNumber input =
  if (Maybe.withDefault -1 (String.toFloat input)) == -1 then
    False
  else
    True


-- VIEW

floatForm model =
  let
    submitButton =
      case (model.validatedNumber && model.validatedRange) of
        True -> smallAppButton SavedInput
        False -> smallAppButtonDisabled

    validationText =
      case model.validatedNumber of
        False -> "Enter a number."
        True ->
          case model.validatedRange of
            False -> "Enter a value between 30 and 45."
            True -> ""

    textDecimal mi =
      if mi == (round mi |> toFloat) then
        Round.round 2 mi
      else
        String.fromFloat mi

    inputStuff unsafe =
      Maybe.withDefault 0 (String.toFloat unsafe)
  in
  h 1 "Temperature"
  :: row [spacing (s 2)]
    [ Input.text [Border.color orangeLight, Font.size (s 3)]
      { label = Input.labelAbove [] (text "celsius" |> el [Font.size (s 1)])
      , onChange = \unsafeInput -> ChangedInput (String.replace "," "." unsafeInput)
      , placeholder = Just (Input.placeholder [] (text "celsius"))
      , text = model.inputString
      }
    , submitButton "log" |> el [alignBottom]
    ]
  :: [UI.p validationText |> el [Font.size (s 1)]]
  |> column [spacing (s 2)]

view : Storage -> Model -> View Msg
view _ model =
  let
    body =
      [ floatForm model |> el [Font.size (s 2)]
      ]
  in
  { title = "log | lab rat"
  , body = body
  }
