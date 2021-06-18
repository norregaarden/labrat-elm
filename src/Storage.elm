port module Storage exposing (..)

import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as Encode
import String exposing (fromInt)
import Time


port save : Json.Value -> Cmd msg
port load : (Json.Value -> msg) -> Sub msg


-- MODEL

type alias Logtype =
  { thenote : String }

type alias Log =
  Dict String Logtype

type alias Person =
  { age : Int
  , height : Int -- cm
  , weight : Int -- kg
  }

type alias Storage =
  { person : Person
  , sometext : String
  , log : Log
  }

initial : Storage
initial =
  { person = Person 0 0 0
  , log = Dict.fromList []
  , sometext = "davdav"
  }


-- UPDATE

newLog : Int -> String -> Storage -> Cmd msg
newLog tid str storage =
    --{ storage | log = Dict.insert tid str storage.log}
    storage
        |> toJson
        |> save

delLog : Storage -> Cmd msg
delLog storage =
    storage
        |> toJson
        |> save

editPerson : Person -> Storage -> Cmd msg
editPerson person storage =
  { storage | person = person }
    |> toJson
    |> save

logsometext : String -> Storage -> Cmd msg
logsometext str storage =
  { storage | sometext = str }
    |> toJson
    |> save


-- SUBSCRIBE

onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)


-- Converting to JSON

toJson : Storage -> Json.Value
toJson storage =
    Encode.object
        [ ("person", Encode.object
          [ ("age", Encode.int storage.person.age)
          , ("height", Encode.int storage.person.height)
          , ("weight", Encode.int storage.person.weight)
          ])
        , ("sometext", Encode.string storage.sometext)

        -- log encoding kan gøres bedre
        , ( "log", Encode.dict identity (\x -> Encode.object [("thenote", Encode.string x.thenote)]) storage.log )
        ]


-- Converting from JSON

fromJson : Json.Value -> Storage
fromJson value =
    value
        |> Json.decodeValue decoder
        |> Result.withDefault initial

decoder : Json.Decoder Storage
decoder =
    Json.map3 Storage
      (Json.field "person" (Json.map3 Person (Json.field "age" Json.int) (Json.field "height" Json.int) (Json.field "weight" Json.int)))
      (Json.field "sometext" Json.string)
      (Json.field "log" (Json.dict logtypeDecoder))

logtypeDecoder : Json.Decoder Logtype
logtypeDecoder =
  Json.map Logtype
    (Json.field "thenote" Json.string)