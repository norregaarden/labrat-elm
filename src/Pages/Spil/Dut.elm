module Pages.Spil.Dut exposing (Model, Msg, page)

import Dutter exposing (..)
import Spil exposing (Score(..))
import Effect exposing (Effect)
import Element exposing (centerX, column, el, fill, fillPortion, height, html, maximum, padding, paddingEach, row, spaceEvenly, spacing, text, width)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Gen.Params.Dut exposing (Params)
import Page
import Random as R
import Random.List exposing (choose, shuffle)
import Request
import Shared
import String exposing (fromInt)
import Task
import Time
import Tuple exposing (first, second)
import UI exposing (appButton, bltr, p, s, showWhen, spilTitel)
import View exposing (View)
import Round


-- SETTINGS

runder : Int
runder = 10

-- TODO
-- varians bliver udregnet med /(n-1)
-- tjek om det er rigtigt


-- INIT

type alias Model =
  { udvalgtDut : Dut
  , dutterne : List Dut
  , igang : Bool
  , tidspunkt : Int
  , klikket : List ( Bool, Int )
  , færdig : Bool
  }


init : ( Model, Effect Msg )
init =
  ( { udvalgtDut = enDut
    , dutterne = []
    , igang = False
    , tidspunkt = 0
    , klikket = []
    , færdig = False
    }
  , Effect.none
  )


-- PAGE

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }




-- UPDATE

type Msg
    = Begynd
    | Blandet (List Dut)
    | Udvælg ( Maybe Dut, List Dut )
    | Start Time.Posix
    | Klik Dut
    | Gem Bool Time.Posix
    | Videre


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  let
    bland =
      R.generate Blandet (shuffle alleDutter)

    udvælg =
      R.generate Udvælg (choose alleDutter)
  in
  case msg of
    Begynd ->
      ( { model | igang = True, færdig = False }, Effect.fromCmd bland )

    Blandet liste ->
      ( { model | dutterne = liste }, Effect.fromCmd udvælg )

    Udvælg whatevs ->
      ( { model | udvalgtDut = fixDut whatevs }, Effect.fromCmd <| Task.perform Start Time.now )

    Start tid ->
      ( { model | tidspunkt = Time.posixToMillis tid }, Effect.none )

    Klik dut ->
      if dut == model.udvalgtDut then
        ( model, Effect.fromCmd <| Task.perform (Gem True) Time.now )
      else ( model, Effect.fromCmd <| Task.perform (Gem False) Time.now )

    Gem bål tid ->
      let
        deltaTid = Time.posixToMillis tid - model.tidspunkt
        nyKlikket = ( bål, deltaTid ) :: model.klikket
        alleRunder = (List.length nyKlikket == runder)

        effect =
          if alleRunder then
            Effect.none
          else
            Effect.fromCmd bland
      in
      ( { model | klikket = nyKlikket, igang = (not alleRunder), færdig = alleRunder }, effect )

    Videre ->
      ( model
      , score model |> DutScore |> Shared.SpilScore |> Effect.fromShared
      )

score model =
  let
    correct =
      List.length (List.filter (first >> (==) True) model.klikket)

    mean =
      round <| List.sum (List.map (second >> toFloat) model.klikket) / (toFloat runder)

    variance =
      List.sum (List.map (\k -> toFloat (second k - mean) ^ 2) model.klikket) / (toFloat runder - 1)
    spread =
      sqrt variance |> round
  in
    { mean = mean
    , spread = spread
    , correct = correct
    , rounds = runder
    }

fixDut : ( Maybe Dut, List Dut ) -> Dut
fixDut ( maybeDut, _ ) =
  case maybeDut of
    Nothing -> enDut
    Just dut -> dut



-- VIEW

topView model =
  if model.igang then
    svgDut model.udvalgtDut |> html
    |> el [width (maximum (s 11) fill), paddingEach (bltr (s 3) (s 3) 0 (s 3))]
    |> el [Border.widthEach (bltr 2 0 0 0)]
  else
    column []
      [ p "Test visual search time" |> el [Font.size (s 3)]
      , column [padding (s 1), spacing (s 1), Font.size (s 1)]
        [ p "Match shape and color." |> el []
        , p (fromInt runder ++ " rounds.") |> el []
        ]
      ]

buttonView model =
    showWhen (not model.igang && not model.færdig) (appButton Begynd "BEGIN")

dataView model =
  let
    scr = score model
    meanString = Round.round 3 <| toFloat scr.mean / 1000
    spreadString = Round.round 3 <| toFloat scr.spread / 1000
  in
  [ row [width fill, spaceEvenly]
    [ "Correct :" |> text |> el [width (fillPortion 1)]
    , fromInt scr.correct ++ " / " ++ fromInt runder |> text |> el [width (fillPortion 1), Font.alignRight]
    ]
  , row [width fill, spaceEvenly]
    [ "Average :" |> text |> el [width (fillPortion 1)]
    , meanString ++ " s" |> text |> el [width (fillPortion 1), Font.alignRight]
    ]
  , row [width fill, spaceEvenly]
    [ "Spread  :" |> text |> el [width (fillPortion 1)]
    , spreadString ++ " s" |> text |> el [width (fillPortion 1), Font.alignRight]
    ]
  , row [width fill]
    [ appButton Videre "Gem" |> el [centerX, padding (s 2)]
    ]
  ]
  |> column [padding (s 3), spacing (s 3), width fill]


dutterView model =
  let
    dutterRow dutter =
      List.map (el [width (fillPortion 1)]) dutter |> row [spacing (s 2), width fill, height fill]

    dutterWrapped dutter =
      if List.length dutter > 0 then
        dutterRow (List.take 4 dutter) :: dutterWrapped (List.drop 4 dutter)
      else []

    huskDutten dut =
      svgDut dut |> html |> el [ onClick (Klik dut) ]
  in
  dutterWrapped (List.map huskDutten model.dutterne)
  |> column [ spacing (s 2), paddingEach (bltr (s 2) (s 2) 0 (s 2)), width fill, height fill ]

view : Model -> View Msg
view model =
  { title = spilTitel "Shapes and colors"
  , body =
    List.map (\e -> el [ centerX ] e)
      [ topView model
      , buttonView model
      , dutterView model |> showWhen model.igang
      ]
    ++ [dataView model |> showWhen model.færdig]
  }