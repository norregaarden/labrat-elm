module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Log
import Request exposing (Request)
import Storage exposing (Storage)
import Task
import Time
import Gen.Route as Route
import Spil exposing (..)

type alias Flags =
    Json.Value

type alias Model =
    { storage : Storage
    , playing :
      { scores : Spil.Scores
      , games : List Spil
      }
    }


type Msg
    = StorageUpdated Storage
    | Play (List Spil)
    | SpilScore Spil.Score



init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { storage = Storage.fromJson flags
      , playing =
        { scores = Spil.Scores Nothing Nothing
        , games = []
        }
    }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
  case msg of
    StorageUpdated storage ->
      ( { model | storage = storage }
      , Request.pushRoute Route.Data req
      )

    Play spilList ->
        case spilList of
          spil::list ->
            ( { model | playing = { scores = Spil.Scores Nothing Nothing, games = list } }
            , Request.pushRoute (Spil.spilRoute spil) req
            )
          [] -> -- will never happen
            ( model, Cmd.none )

    SpilScore score ->
      let
        playing = model.playing
        newPlaying =
          { playing
            | scores = Spil.updateScores playing.scores score
            , games = Spil.updateGames playing.games score
          }
        command =
          case newPlaying.games of
            spil::list ->
              Request.pushRoute (Spil.spilRoute spil) req
            [] ->
              Request.pushRoute Route.Data req
      in
        ( {model | playing = newPlaying }, command )

subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    --Sub.none
    Storage.onChange StorageUpdated
