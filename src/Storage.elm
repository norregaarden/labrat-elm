port module Storage exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Extra exposing (dict2)
import Json.Encode as E
import String exposing (fromInt)
--import Time
import Log exposing (Data(..), Weight(..))
import Time

port save : D.Value -> Cmd msg
port load : (D.Value -> msg) -> Sub msg


-- MODEL
{-
type alias Logtype =
  { thenote : String }

type alias Log =
  Dict String Logtype
-}

type alias Person =
  { years : Int
  , cm : Int
  , kg : Int
  }

--type alias Tid = Time.Posix

type alias Storage =
  { person : Person
  , sometext : String
  , log : Dict Int Data
  }

initial : Storage
initial =
  { person = Person 0 0 0
  , sometext = "davdav"
  , log = Dict.empty
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

logData : Int -> Data -> Storage -> Cmd msg
logData tidms data storage =
  { storage | log = Dict.insert tidms data storage.log }
    |> toJson
    |> save

-- SUBSCRIBE

onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> fromJson json |> fromStorage)


-- Converting to JSON

toJson : Storage -> E.Value
toJson storage =
    E.object
        [ ("person", E.object
          [ ("years", E.int storage.person.years)
          , ("cm", E.int storage.person.cm)
          , ("kg", E.int storage.person.kg)
          ])
        , ("sometext", E.string storage.sometext)

        -- log encoding kan gøres bedre
        , ( "log", E.dict String.fromInt encodeData storage.log )
        ]


encodeData : Data -> E.Value
encodeData data =
  case data of
    TempC c ->
      E.object [( "TempC", E.int c )]
    HR hr ->
      E.object [( "HR", E.int hr )]
    BP high low ->
      E.object [( "BP", E.object [("high", E.int high), ("low", E.int low)] )]
    Musing text ->
      E.object [( "Musing", E.string text )]
    Intox drug weight ->
      E.object [( "Intox", E.object [("drug", E.string drug), encodeWeight weight] )]

encodeWeight : Weight -> (String, E.Value)
encodeWeight weight =
  case weight of
    Microgram ug ->
      ("microgram", E.int ug)
    Milligram mg ->
      ("milligram", E.int mg)
    Gram g ->
      ("gram", E.int g)

--(\x -> E.object [("thenote", E.string x.thenote)])

-- Converting from JSON

fromJson : D.Value -> Storage
fromJson value =
    value
        |> D.decodeValue decoder
        |> Result.withDefault initial

decoder : D.Decoder Storage
decoder =
    D.map3 Storage
      (D.field "person" personDecoder)
      (D.field "sometext" D.string)
      (D.field "log" (dict2 D.int dictDataDecoder ))

personDecoder : D.Decoder Person
personDecoder =
  (D.map3 Person (D.field "years" D.int) (D.field "cm" D.int) (D.field "kg" D.int))

dictDataDecoder : D.Decoder Data
dictDataDecoder =
  D.oneOf
    [ (D.field "HR" (D.map HR D.int))
    ]

--listDataDecoder : D.Decoder (List Data)
--listDataDecoder =
--  D.map List
--    (D.field "thenote" D.string)
--