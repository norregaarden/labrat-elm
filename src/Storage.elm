port module Storage exposing (..)

import Browser.Navigation
import Dict exposing (Dict)
import Json.Decode as D exposing (decodeString)
import Json.Decode.Extra as Dextra
import Json.Encode as E
import Json.Encode.Extra as Eextra
import Log exposing (Data(..), ROA, Weight(..), WeightUnit(..))
import Spil exposing (Score(..), Scores)

port save : E.Value -> Cmd msg
port load : (D.Value -> msg) -> Sub msg


-- MODEL

type alias Person =
  { years : Int
  , cm : Int
  , kg : Int
  }


type alias Storage =
  { identifier : String
  , person : Person
  , log : Dict Int DataLog
  , playlog : Dict Int Scores
  }

type alias DataLog =
  { time : Int
  , data : Data
  }

initial : Storage
initial =
  { identifier = ""
  , person = Person 0 0 0
  , log = Dict.empty
  , playlog = Dict.empty
  }

setIdentifier : String -> Storage -> Cmd msg
setIdentifier id storage =
  if storage.identifier == "" then
    { storage | identifier = id }
      |> toJson
      |> save
  else
    Cmd.none


-- UPLOAD (from .json to localstorage)

uploadJson jsonstr =
  let
    result = decodeString decoder jsonstr
    dothis = case result of
      Ok value ->
        save (toJson value)

      Err error ->
        Cmd.none -- replace with popup saying wrong version data?
        -- make cmd in shared, general popup uses var error

  in
  Cmd.batch [ dothis, Browser.Navigation.reload ]



-- UPDATE

editPerson : Person -> Storage -> Cmd msg
editPerson person storage =
  { storage | person = person }
    |> toJson
    |> save

logData : Int -> (Int, Data) -> Storage -> Cmd msg
logData ms_log (ms_data, data) storage =
  { storage | log = Dict.insert ms_log { time = ms_data, data = data } storage.log }
    |> toJson
    |> save

logScores : Int -> Scores -> Storage -> Cmd msg
logScores tidms scores storage =
  { storage | playlog = Dict.insert tidms scores storage.playlog }
    |> toJson
    |> save


-- DELETE

deleteData : Int -> Storage -> Cmd msg
deleteData ms_log storage =
  { storage | log = Dict.remove ms_log storage.log }
    |> toJson
    |> save

deleteScores : Int -> Storage -> Cmd msg
deleteScores tidms storage =
  { storage | playlog = Dict.remove tidms storage.playlog }
    |> toJson
    |> save


-- SUBSCRIBE

onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
  load (\json -> fromJson json |> fromStorage)


---------------------
-- Converting to JSON

toJson : Storage -> E.Value
toJson storage =
  E.object
    [ ("identifier", E.string storage.identifier)
    , ("person", E.object
      [ ("years", E.int storage.person.years)
      , ("cm", E.int storage.person.cm)
      , ("kg", E.int storage.person.kg)
      ])
    , ("log", E.dict String.fromInt encodeData storage.log)
    , ("playlog", E.dict String.fromInt encodeScores storage.playlog)
    ]


-- encode LOG

encodeData : DataLog -> E.Value
encodeData datalog =
  let
    time = E.int datalog.time
    data =
      case datalog.data of
        TempC c ->
          E.object [( "TempC", E.float c )]
        HR hr ->
          E.object [( "HR", E.int hr )]
        BP high low ->
          E.object [( "BP", E.object [("high", E.int high), ("low", E.int low)] )]
        Musing text ->
          E.object [( "Musing", E.string text )]
        DrugAdmin drug roa weight ->
          E.object [( "DrugAdmin", E.object
            [ ("drug", E.string drug)
            , ("roa", E.string (Log.text_roa roa))
            , ("weight", encodeWeight weight)]
            )]
  in
  E.object
    [ ("time", time)
    , ("data", data)
    ]

encodeWeight : Weight -> E.Value
encodeWeight weight =
  case weight of
    Quan unit amount ->
      E.object
        [ ("unit", E.string (Log.text_wu unit))
        , ("amount", E.int amount)
        ]
    Qual qualifier ->
      E.object
        [ ("qualitative", E.string (Log.text_dq qualifier))
        ]


-- encode PLAY

encodeScores : Scores -> E.Value
encodeScores scores =
  let
    dutEncode =
      case scores.dut of
        Just s -> encodeScore (DutScore s)
        Nothing -> E.null
    tidEncode =
      case scores.tid of
        Just s -> encodeScore (TidScore s)
        Nothing -> E.null
    huskEncode =
      case scores.husk of
        Just s -> encodeScore (HuskScore s)
        Nothing -> E.null
  in
  E.object
    [ ("dut", dutEncode )
    , ("tid", tidEncode )
    , ("husk", huskEncode )
    ]

encodeScore : Score -> E.Value
encodeScore score =
  case score of
    DutScore s ->
      E.object
        [ ("mean", E.int s.mean)
        , ("spread", E.int s.spread)
        , ("correct", E.int s.correct)
        , ("rounds", E.int s.rounds)
        ]

    TidScore s ->
      E.object
        [ ("burde", E.int s.burde)
        , ("faktisk", E.int s.faktisk)
        ]

    HuskScore s ->
      E.object
        [ ("huskNumber", E.int s.huskNumber)
        , ("totalMistakes", E.int s.totalMistakes)
        ]

    BlinkScore s ->
      E.object
        [ ("expectedDuration_ms", E.int s.expectedDuration_ms)
        , ("realDuration_ms", E.int s.realDuration_ms)
        ]


-----------------------
-- Converting from JSON

fromJson : D.Value -> Storage
fromJson value =
  value
    |> D.decodeValue decoder
    |> Result.withDefault initial

decoder : D.Decoder Storage
decoder =
  D.map4 Storage
    (D.field "identifier" D.string)
    (D.field "person" personDecoder)
    (D.field "log" (Dextra.dict2 D.int dictDataDecoder ))
    (D.field "playlog" (Dextra.dict2 D.int dictScoresDecoder ))

personDecoder : D.Decoder Person
personDecoder =
  (D.map3 Person (D.field "years" D.int) (D.field "cm" D.int) (D.field "kg" D.int))

dictDataDecoder : D.Decoder DataLog
dictDataDecoder =
  let
    dataDecoder =
      D.oneOf
        [ (D.field "BP" bpDecoder)
        , (D.field "HR" (D.map HR D.int))
        , (D.field "TempC" (D.map TempC D.float))
        , (D.field "DrugAdmin" drugDecoder)
        , (D.field "Musing" (D.map Musing D.string))
        ]
  in
  D.map2 DataLog
    ( D.field "time" D.int )
    ( D.field "data" dataDecoder )

bpDecoder : D.Decoder Data
bpDecoder =
  D.map2 BP
    ( D.field "high" D.int )
    ( D.field "low" D.int )

drugDecoder : D.Decoder Data
drugDecoder =
  let
    roaDecoder : D.Decoder ROA
    roaDecoder =
      D.map Log.roa_from D.string

    weightDecoder : D.Decoder Weight
    weightDecoder =
      D.oneOf
        [ D.map2 Quan
            (D.field "unit" (D.map Log.wu_from D.string))
            (D.field "amount" (D.int))
        , D.map Qual
            (D.field "qualitative" (D.map Log.dq_from D.string))
        ]
  in
  D.map3 DrugAdmin
    (D.field "drug" D.string)
    (D.field "roa" roaDecoder)
    (D.field "weight" weightDecoder)

dictScoresDecoder : D.Decoder Scores
dictScoresDecoder =
  D.map4 Scores
    (D.field "dut" (D.nullable scoreDutDecoder))
    (D.field "tid" (D.nullable scoreTidDecoder))
    (D.field "husk" (D.nullable scoreHuskDecoder))
    (D.field "blink" (D.nullable scoreBlinkDecoder))

scoreDutDecoder : D.Decoder Spil.Score_Dut
scoreDutDecoder =
  D.map4 Spil.Score_Dut
    (D.field "mean" D.int)
    (D.field "spread" D.int)
    (D.field "correct" D.int)
    (D.field "rounds" D.int)

scoreTidDecoder : D.Decoder Spil.Score_Tid
scoreTidDecoder =
  D.map2 Spil.Score_Tid
    (D.field "burde" D.int)
    (D.field "faktisk" D.int)

scoreHuskDecoder : D.Decoder Spil.Score_Husk
scoreHuskDecoder =
  D.map2 Spil.Score_Husk
    (D.field "huskNumber" D.int)
    (D.field "totalMistakes" D.int)

scoreBlinkDecoder : D.Decoder Spil.Score_Blink
scoreBlinkDecoder =
  D.map2 Spil.Score_Blink
    (D.field "expectedDuration_ms" D.int)
    (D.field "realDuration_ms" D.int)
