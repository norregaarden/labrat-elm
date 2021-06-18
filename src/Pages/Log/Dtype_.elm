module Pages.Log.Dtype_ exposing (Model, Msg, page)

import Dict exposing (Dict)
import Element exposing (alignBottom, column, el, row, spacing, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Log.Dtype_ exposing (Params)
import Page
import Request
import Shared
import Storage exposing (Storage)
import UI exposing (appButton, p, s, smallAppButton)
import UIColor exposing (orangeLight)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view shared.storage
        , subscriptions = \_ -> Sub.none
        }

-- SUBSCRIPTIONS
{-
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    -}


-- INIT


type alias Model =
    { textInput : String
    }


init : ( Model, Cmd Msg )
init = (
    { textInput = ""
    }, Cmd.none )



-- UPDATE


type Msg
    = TextInput String
    | SaveText
    --| EditAge String
    --| EditHeight String
    --| EditWeight String


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        TextInput str ->
            ( { model | textInput = str }, Cmd.none )
        SaveText ->
            ( model, Storage.logsometext model.textInput storage )



-- VIEW

--introduktion log = String.fromInt (Dict.size log) ++ " items in log" |> p |> el [Font.size (s 3)]

formular model =
  [ Input.text [Border.color orangeLight]
    { label = Input.labelAbove [] (text "new string value" |> el [Font.size (s 1)])
    , onChange = TextInput
    , placeholder = Just (Input.placeholder [] (text "write here"))
    , text = model.textInput
    }
  , smallAppButton SaveText "LOG" |> el [alignBottom]
  ]
  |> row [spacing (s 1)]

view : Storage -> Model -> View Msg
view storage model =
    { title = "log | lab rat"
    , body =
      [ --introduktion log
      formular model |> el [Font.size (s 2)]
      , text ""
      , text ""
      , text ""
      , text "___________________"
      , p model.textInput |> el [Font.size (s 1)]
      , p (Debug.toString storage.sometext) |> el [Font.size (s 1)]
      , p (Debug.toString  storage.log) |> el [Font.size (s 1)]
      ]
      |> UI.appLayout
    }
