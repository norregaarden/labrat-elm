module Pages.Log.Musing exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (alignTop, centerX, column, el, fill, height, minimum, paddingEach, paddingXY, px, text, width)
import Element.Input as Input
import Gen.Params.Log.Musing exposing (Params)
import Log exposing (Data(..))
import Page
import Request
import Shared
import Storage exposing (Storage)
import Task
import Time
import UI exposing (h, s, smallAppButton)
import View exposing (View)
import Page


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update shared.storage
    , view = view
    , subscriptions = \_ -> Sub.none
    }



-- INIT


type alias Model =
  { input : String
  }


init : ( Model, Effect Msg )
init =
  ( Model ""
  , Effect.none
  )



-- UPDATE


type Msg
    = ChangedInput String
    | SavedInput
    | LogDataTid Time.Posix


update : Storage -> Msg -> Model -> ( Model, Effect Msg )
update storage msg model =
  case msg of
    ChangedInput input ->
      ( { model | input = input }, Effect.none )
    SavedInput ->
      ( model, Task.perform LogDataTid Time.now |> Effect.fromCmd )
    LogDataTid tid ->
      ( model
      , Effect.fromCmd <|
        Storage.logData (Time.posixToMillis tid) (Time.posixToMillis tid, Musing model.input) storage
      )



-- VIEW


vis model =
  [ h 1 "Musing"
  , Input.multiline []
    { onChange = ChangedInput
    , text = model.input
    , placeholder = Just (Input.placeholder [] (text "How do you feel?"))
    , label = Input.labelHidden "How do you feel?"
    , spellcheck = True
    }
  , smallAppButton SavedInput "save" |> el [centerX, paddingXY 0 (s 8)]
  ]


view : Model -> View Msg
view model =
  { title = "log | lab rat"
  , body = vis model
  }
