module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)
import Storage exposing (Storage)

type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage }


type Msg
    = StorageUpdated Storage


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { storage = Storage.fromJson flags }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage ->
            ( { model | storage = storage }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    --Sub.none
    Storage.onChange StorageUpdated
