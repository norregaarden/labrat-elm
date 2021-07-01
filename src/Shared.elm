module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
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
  , playing : Maybe PlayingModel
  }

type alias PlayingModel =
  { scores : Spil.Scores
  , games : List Spil
  }


type Msg
    = StorageUpdated Storage
    | Play (List Spil)
    | SpilScore Spil.Score
    | SaveScores Time.Posix
    | GoToPlay



init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
  ( { storage = Storage.fromJson flags
    , playing = Nothing
  }, Cmd.none )

initPlaying =
  { scores = Spil.Scores Nothing Nothing Nothing
  , games = []
  }


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
          ( { model | playing = Just
              { scores = initPlaying.scores
              , games = spil::list }
              }
          , Request.pushRoute (Spil.spilRoute spil) req
          )
        [] -> -- will never happen
          ( model, Cmd.none )

    SpilScore score ->
      let
        playing =
          case model.playing of
            Nothing -> initPlaying
            Just p -> p

        newPlaying =
          { scores = Spil.updateScores playing.scores score
          , games = Spil.updateGames playing.games score
          }

        command =
          case newPlaying.games of
            spil::list ->
              Request.pushRoute (Spil.spilRoute spil) req
            [] ->
              Task.perform SaveScores Time.now
      in
        ( {model | playing = Just newPlaying }, command )

    SaveScores tid ->
      case model.playing of
        Nothing ->
          ( model, Cmd.none )
        Just p ->
          ( { model | playing = Nothing }
          , Storage.logScores (Time.posixToMillis tid) p.scores model.storage
          )

    GoToPlay ->
      ( model, Request.pushRoute Route.Play req)



subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
  Storage.onChange StorageUpdated
