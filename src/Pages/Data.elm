module Pages.Data exposing (Model, Msg, page)

import Element exposing (text)
import Gen.Params.Data exposing (Params)
import Page
import Request
import Shared
import UI exposing (p)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
  Page.element
    { init = init
    , update = update
    , view = view shared
    , subscriptions = \_ -> Sub.none
    }



-- INIT


type alias Model =
  {}


init : ( Model, Cmd Msg )
init =
  ( {}, Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ReplaceMe ->
      ( model, Cmd.none )



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
  { title = "data | lab rat"
  , body =
    [ text "shared.playing"
    --, p (Debug.toString shared.playing)
    , text "shared.storage"
    , p (Debug.toString shared.storage.playlog)
    ]
  }
