module Pages.Play exposing (Model, Msg, page)

import Effect exposing (Effect)
import Random
import Random.List
import Spil exposing (..)
import Element exposing (centerX, column)
import Gen.Params.Play exposing (Params)
import Page
import Request
import Shared
import UI exposing (..)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- INIT


type alias Model =
  {}


init : ( Model, Effect Msg )
init =
  ( {}
  , Effect.none
  )



-- UPDATE


type Msg
    = PlayClick
    | PlayList (List Spil)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    PlayClick ->
      ( model
      , Effect.fromCmd <|
        Random.generate PlayList (Random.List.shuffle Spil.alleSpil)
      )
    PlayList list ->
      ( model
      , Effect.fromShared <|
        Shared.Play list
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> View Msg
view model =
  { title = "play | lab rat"
  , body =
    column [centerX]
      [ appButton PlayClick "Play"
      ]
    |> List.singleton
  }
