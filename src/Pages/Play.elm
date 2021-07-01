module Pages.Play exposing (Model, Msg, page)

import Dutter exposing (svgDut)
import Effect exposing (Effect)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Route as Route
import Husk
import Random
import Random.List
import Spil exposing (..)
import Element exposing (centerX, centerY, column, el, fill, fillPortion, height, html, link, padding, row, spacing, text, width)
import Gen.Params.Play exposing (Params)
import Page
import Request
import Shared
import UI exposing (..)
import UIColor exposing (orangeLight)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }



-- INIT


type alias Model =
  { dutList : List Dutter.Dut
  , huskList : List Husk.Image
  }


init : ( Model, Effect Msg )
init =
  ( Model [] []
  , Effect.fromCmd
    <| Random.generate RandomLists
    <| Random.pair
      (Random.List.shuffle Dutter.alleDutter)
      (Random.List.shuffle Husk.allImages)
  )



-- UPDATE


type Msg
    = RandomLists (List Dutter.Dut, List Husk.Image)
    | PlayClick
    | PlayList (List Spil)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    RandomLists (dutterne, huskene) ->
      ( { dutList = dutterne
        , huskList = huskene }
      , Effect.none )

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



-- VIEW


viewDutter dutterne =
  let
    space = (s -5)

    dutterRow dutter =
      List.map (el [width (fillPortion 1)]) dutter |> row [ width fill, height fill ]

    dutterWrapped dutter =
      if List.length dutter > 0 then
        dutterRow (List.take 4 dutter) :: dutterWrapped (List.drop 4 dutter)
      else []

    dutElement dut =
      svgDut dut |> html |> el [ padding space ]
  in
  dutterWrapped (List.map dutElement dutterne)
  |> column [width fill, height fill ]


viewHuskImages huskene =
  let
    billederRow billeder =
      List.map (el [width (fillPortion 1)]) billeder
      |> row [width fill, height fill]

    billederWrapped billeder =
      if List.length billeder > 0 then
        billederRow (List.take 3 billeder) :: billederWrapped (List.drop 3 billeder)
      else []

    imageProperties billede =
      { src = "/images/husk/" ++ Husk.imageString billede ++ ".svg"
      , description = Husk.imageString billede
      }

    billedeElement billede =
      el [Border.rounded 123] <|
      Element.image
        [width fill, padding (s -7)]
        (imageProperties billede)

  in
  billederWrapped (List.map billedeElement huskene)
  |> column [ width fill, height fill ]


view : Model -> View Msg
view model =
  { title = "play | lab rat"
  , body =
    [ p "Play all games in random order and save the results:"
    , appButton PlayClick "Play" |> el [centerX]
    , small "About one minute in total."
    , text ""
    , text ""
    , text ""
    , p "Or casually try one of the three games:"
    , row [width fill, spacing (s -3)] <|
      List.map
        (\l -> link [centerX, centerY, padding (s -3)] l
          |> el [width (fillPortion 1), height fill
            , Background.color orangeLight, Border.rounded (s 1)]
        )
        [ { url = Route.toHref Route.Spil__Dut
          , label = viewDutter model.dutList
          }
        , { url = Route.toHref Route.Spil__Tid
          , label = column [centerX] <|
              List.map (el [centerX])
                [ p "10" |> el [Font.size (s 6)
                , Font.extraBold], p "seconds"]
          }
        , { url = Route.toHref Route.Spil__Husk
          , label = viewHuskImages model.huskList
          }
        ]
    ]
  }