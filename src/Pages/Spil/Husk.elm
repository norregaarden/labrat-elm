module Pages.Spil.Husk exposing (Model, Msg, page)

import Element.Background as Background
import Element.Border as Border
import Husk
import Effect exposing (Effect)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, padding, paddingXY, row, text, width)
import Element.Events as Events
import Element.Font as Font
import Page
import Process
import Random
import Random.List
import Request exposing (Request)
import Shared
import Spil exposing (Score(..), Score_Husk)
import Task
import UI exposing (appButton, h, p, s, small)
import UIColor exposing (green, red)
import View exposing (View)


-- SETTINGS

startHuskNumber
  = 2

imageShowTime
  = 666 * 1.5 --ms

maxMistakes
  = 2

countFrom
  = 3

countdownTime
  = 666 * 1.5 --ms


-- PAGE


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update
    , view = view shared
    , subscriptions = \_ -> Sub.none
    }



-- INIT


type Model
  = Intro
  | Igang IgangModel
  | Død IgangModel
  | Error String


type alias IgangModel =
  { tilstand : IgangTilstand
  , huskNummer : Int
  , klikkedeBilleder : List Husk.Image
  , klikForkert : Maybe Husk.Image
  , udvalgteBilleder : List Husk.Image
  , alleBilleder : List Husk.Image
  , fejl : Int
  , totalFejl : Int
  }


type IgangTilstand
  = TreToEn Int
  | Billede (List Husk.Image)
  --| Ventende
  | KlikView


init : ( Model, Effect Msg )
init =
  ( Intro, Effect.none )


igangInit : IgangModel
igangInit =
  { tilstand = igangTilstandInit
  , huskNummer = startHuskNumber
  , klikkedeBilleder = []
  , klikForkert = Nothing
  , udvalgteBilleder = []
  , alleBilleder = []
  , fejl = 0
  , totalFejl = 0
  }

igangTilstandInit = TreToEn countFrom


-- UPDATE


type Msg
    = Begynd
    | Blandet (List Husk.Image)
    | Udvalgte (List Husk.Image)
    | Countdown
    | Klik Husk.Image
    | Klog
    | Dum
    | Videre
    | OK


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  let
    blandBillederne =
      Effect.fromCmd <|
        Random.generate Blandet (Random.List.shuffle Husk.allImages)
  in
  case model of
    Intro ->
      case msg of
        Begynd ->
          ( Igang igangInit
          , blandBillederne
          )

        _ ->
          ( Error "Intro", Effect.none )

    Igang igang ->
      case msg of
        Blandet liste ->
          ( Igang { igang
            | alleBilleder = liste
            }
          , Effect.fromCmd <|
              Random.generate Udvalgte (Random.List.shuffle Husk.allImages)
          )

        Udvalgte liste ->
          ( Igang { igang
            | udvalgteBilleder = List.take igang.huskNummer liste
            }
          , countdown Countdown
          )

        Countdown ->
          case igang.tilstand of
            TreToEn n ->
              if n > 1 then
                ( Igang { igang
                  | tilstand = TreToEn (n - 1)
                  }
                , countdown Countdown
                )

              else
                ( Igang { igang
                  | tilstand = Billede igang.udvalgteBilleder
                  }
                , wait Countdown
                )

            Billede l ->
              let
                vent =
                  ( Igang { igang
                    | tilstand = KlikView
                    }
                  , Effect.none
                  )
              in
              case List.tail l of
                Just (b::list) ->
                  ( Igang { igang
                    | tilstand = Billede (b::list)
                    }
                  , wait Countdown
                  )

                Just list ->
                  vent

                Nothing ->
                  vent
{-
            Ventende ->
              ( Igang { igang
                | tilstand = KlikView
                }
              , Effect.none
              )
-}
            KlikView ->
              ( Error "Igang / tilstand KlikView / msg Countdown", Effect.none )

        Klik billede ->
          case igang.tilstand of
            KlikView ->
              let
                newKlikkede = igang.klikkedeBilleder ++ [billede]
                numberKlikkede = List.length newKlikkede
                compareTo = List.take numberKlikkede igang.udvalgteBilleder
                rigtigt =
                  newKlikkede == compareTo
                alleKlikket =
                  List.length newKlikkede == igang.huskNummer

                newModel =
                  if rigtigt then
                    Igang { igang
                      | klikkedeBilleder = newKlikkede
                      }
                  else
                    Igang { igang
                      | klikForkert = Just billede
                      }

                effect =
                  if rigtigt && alleKlikket then
                    wait Klog
                  else if not rigtigt then
                    wait Dum
                  else Effect.none
              in
              ( newModel, effect )

            _ ->
              ( Error "Igang / msg Klik / tilstand not KlikView", Effect.none )

        Klog ->
          ( Igang { igang
            | tilstand = igangTilstandInit
            , fejl = 0
            , huskNummer = igang.huskNummer + 1
            , klikkedeBilleder = []
            , klikForkert = Nothing
            }
          , blandBillederne
          )

        Dum ->
          let
            newFejl = igang.fejl + 1
            newIgang = { igang
              | tilstand = igangTilstandInit
              , fejl = newFejl
              , totalFejl = igang.totalFejl + 1
              , klikkedeBilleder = []
              , klikForkert = Nothing
              }
          in
          if newFejl < maxMistakes then
            ( Igang newIgang
            , blandBillederne
            )
          else
            ( Død newIgang
            , Effect.none
            )

        _ -> -- Begynd, Videre, OK
          ( Error "Igang", Effect.none )


    Død igang ->
      case msg of
        Videre ->
          ( model
          , score igang |> HuskScore |> Shared.SpilScore |> Effect.fromShared
          )

        OK ->
          ( model
          , Shared.GoToPlay |> Effect.fromShared
          )

        _ ->
          ( model, Effect.none )

    Error string ->
      ( Error (string ++ " then Error"), Effect.none )


countdown : Msg -> Effect Msg
countdown msg =
  Process.sleep countdownTime
  |> Task.perform (\_ -> msg)
  |> Effect.fromCmd

wait msg =
  Process.sleep imageShowTime
  |> Task.perform (\_ -> msg)
  |> Effect.fromCmd



score : IgangModel -> Score_Husk
score igang =
  if igang.huskNummer == List.length igang.udvalgteBilleder then
    if igang.huskNummer > startHuskNumber then
      { huskNumber = igang.huskNummer
      , totalMistakes = igang.totalFejl
      }
    else
      { huskNumber = 0
      , totalMistakes = igang.totalFejl
      }
  else
    { huskNumber = -1
    , totalMistakes = -1
    }



-- VIEW

imageProperties billede =
  { src = "/images/husk/" ++ Husk.imageString billede ++ ".svg"
  , description = Husk.imageString billede
  }

viewImages igang =
  let
    billederRow billeder =
      List.map (el [width (fillPortion 1)]) billeder
      |> row [width fill, height fill]

    billederWrapped billeder =
      if List.length billeder > 0 then
        billederRow (List.take 3 billeder) :: billederWrapped (List.drop 3 billeder)
      else []

    klikBillede billede =
      let
        attrs =
          if List.member billede igang.klikkedeBilleder then
            [Background.color green]
          else if Just billede == igang.klikForkert then
            [Background.color red]
          else
            []

        click =
          case igang.klikForkert of
            Nothing ->
              [Events.onClick (Klik billede)]
            Just _ ->
              []
      in
      el (Border.rounded 123 :: attrs) <|
      Element.image
        (click ++ [width fill, padding (s -2)])
        (imageProperties billede)

  in
  billederWrapped (List.map klikBillede igang.alleBilleder)
  |> column [ width fill, height fill ]


visIgang igang =
  let
    content =
      case igang.tilstand of
        TreToEn n ->
          let
            toptext =
              if igang.fejl > 0 then
                String.fromInt (maxMistakes - igang.fejl)
                ++ " tries left this round."
              else
                ""
          in
          column [centerX]
            [ toptext
              |> p |> el [Font.color red, Font.extraBold, paddingXY 0 (s 2)]
            , text (String.fromInt n)
              |> el [centerX, padding (s 6), Font.size (s 8)
                --, Border.rounded 123, Border.width 1, width (px 200), height (px 200)
                ]
            , String.fromInt igang.huskNummer ++ " images this round."
              |> p |> el [Font.color green, Font.extraBold, paddingXY 0 (s 2)]
            ]

        Billede [] ->
          text "Error - no image to show"

        Billede (billede::_) ->
          Element.image
            [ width fill, paddingXY 0 (s 3) ]
            (imageProperties billede)
          |> el [centerX, centerY]

{-
        Ventende ->
          text "Ventende"
          |> el [centerX, centerY]
-}

        KlikView ->
          viewImages igang
  in
  [content]


vis sharedPlaying model =
  let
    overskrift = h 2 "Test your short term memory"
  in
  case model of
    Intro ->
      [ overskrift
      , small <|
          "Some images will flash sequentially. "
      , small <|
          "Afterwards, click the images in the corresponding order."
      --, small <| String.fromInt startHuskNumber ++ " images in the first round."
      , text ""
      , appButton Begynd "READY" |> el [centerX]
      --, viewImages Husk.allImages False
      ]

    Igang igang ->
      visIgang igang

    Død igang ->
      let
        summary = score igang
      in
      [ overskrift
      , p <| "You can hold " ++ String.fromInt summary.huskNumber ++ " items in short-term memory."
      , small <| "You made " ++ String.fromInt summary.totalMistakes ++ " mistakes in total."
      , Spil.videreButton sharedPlaying Videre OK
      ]
      --|> text |> el [padding (s 6)] |> List.singleton

    Error string ->
      "ERROR from husk model " ++ string ++ ", msg not applicable"
      |> p |> el [padding (s 2)] |> List.singleton

  --billede "noegenhat" |> el [padding 40] |> List.singleton


view : Shared.Model -> Model -> View Msg
view shared model =
  { title = "Spil: Husk"
  , body = vis shared.playing model
  }