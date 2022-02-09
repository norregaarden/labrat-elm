module Pages.Spil.Blink exposing (Model, Msg, page)

import App.Blink as Blink exposing (Image, JSimage, blink, imageProperties, placeholderImage)
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Effect exposing (Effect)
import Element exposing (Element, centerX, centerY, column, el, fill, htmlAttribute, padding, paddingXY, spacing, text, width)
import Element.Font as Font
import Html.Attributes
import Json.Encode as E
import Page
import Random
import Random.List
import Request exposing (Request)
import Round
import Shared
import Spil exposing (Score(..))
import String exposing (fromFloat, fromInt)
import Task
import Time
import UI exposing (appButton, h, p, s, showListWhen, showWhen, small, spilTitel)
import View exposing (View)


blinkTimes = 3

-- MODEL

type Model
  = Intro
  | Igang IgangModel
  | Død IgangModel
  | Error String


type alias IgangModel =
  { duration : Int --ms
  , image : Image
  , jsImage : JSimage
  --, frameFuture : List String
  , frameHistory : List (String, Time.Posix)
  }


igangModelPlaceholder =
  { duration = 5
  , image = placeholderImage
  , jsImage = imageProperties placeholderImage
  , frameHistory = []
  }


init : ( Model, Effect Msg )
init =
  ( Intro
  , Effect.none )


-- BOILERPLATE

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update
    , view = view shared
    , subscriptions =
        \model ->
            case model of
                Igang _ ->
                    onAnimationFrame Frame
                _ ->
                    Sub.none
        --\model -> onAnimationFrameDelta Frame
    }


-- UPDATE

type Msg
    = Begynd
    | Blandet (Maybe Image, List Image)
    | Frame Time.Posix
{-    = Startklik
    | Start Time.Posix
    | Slutklik
    | Slut Time.Posix-}
    | Videre
    | OK


toJson : IgangModel -> E.Value
toJson igang =
  E.object
    [ ("duration", E.int igang.duration)
    , ("jsImage", E.object
      [ ("src", E.string igang.jsImage.src)
      , ("description", E.string igang.jsImage.description)
      ])
    ]

update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  let
    randomImage =
      Effect.fromCmd <|
        Random.generate Blandet (Random.List.choose Blink.allImages)
  in
  case model of
    Intro ->
      case msg of
        Begynd ->
          ( Igang igangModelPlaceholder
          , randomImage
          )

        _ ->
          ( Error "Intro", Effect.none )

    Igang igang ->
      case msg of
        Blandet (maybeImage, _) ->
          ( Igang { igang
            | image = Maybe.withDefault placeholderImage maybeImage
            , jsImage = imageProperties (Maybe.withDefault placeholderImage maybeImage)
            }
          , Effect.fromCmd <| blink (toJson igang)
          --, Effect.fromCmd <| Random.generate Udvalgte (Random.List.shuffle Husk.allImages)
          )

        Frame time ->
          let
            newStr =
              igang.jsImage.description

            newHistory =
              case igang.frameHistory of
                  [] ->
                      [ ("", time) ]
                  (oldStr, oldTime) :: fs ->
                      (newStr, time) :: igang.frameHistory
{-                    if newStr /= oldStr then
                      (newStr, time) :: igang.frameHistory
                    else
                      igang.frameHistory-}

            newIgang =
              { igang
              | frameHistory = newHistory
              }

          in
            if List.length igang.frameHistory < 2 then
              --( Igang newIgang, randomImage )
              ( Igang newIgang, Effect.none )
            else
              ( Død newIgang, Effect.none )

        _ ->
          ( Error "Igang / ikke Blandet", Effect.none )

    Død igang ->
      case msg of
        Videre ->
          ( model
          , score model |> BlinkScore |> Shared.SpilScore |> Effect.fromShared
          )

        OK ->
          ( model
          , Shared.GoToPlay |> Effect.fromShared
          )

        _ ->
          ( Error "Død / ikke Videre eller OK", Effect.none )

    Error string ->
      ( Error (string ++ " then Error"), Effect.none )


score model =
  case model of
    Igang igangModel ->
      { expectedDuration_ms = igangModel.duration
      , realDuration_ms = igangModel.duration
      }

    _ ->
      { expectedDuration_ms = -1
      , realDuration_ms = -1
      }



-- HELPERS




-- VIEW

visIgang : IgangModel -> List (Element msg)
visIgang igang =
  let
    content =
      Element.image
        [ width fill
        , paddingXY 0 (s 3)
        --, Html.Attributes.id "blinkImage" |> htmlAttribute
        --, Html.Attributes.style "opacity" "0.001" |> htmlAttribute
        ]
        (igang.jsImage)
      |> el [centerX, centerY]
  in
  [content]


vis sharedPlaying model =
  let
    titelskrift = el [Font.extraBold] <| text "blink"
    overskrift = h 2 "Find your minimal recognition time"
  in
  case model of
    Intro ->
      [ titelskrift
      , overskrift
      , small <|
          "An image will blink " ++ String.fromInt blinkTimes ++  " times for a short amount of time."
      , small <|
          "Afterwards, click the corresponding image."
      , text ""
      , appButton Begynd "READY" |> el [centerX]
      --, viewImages Husk.allImages False
      ]

    Igang igang ->
      visIgang igang

    Død igang ->
      let
        --summary = score igang
        hej = "hej"

        frameHisStr (imgStr, timePosix) =
            (Time.posixToMillis timePosix |> String.fromInt)
            ++ ": " ++ imgStr
      in
      [ overskrift
      --, p <| "You can hold " ++ String.fromInt summary.huskNumber ++ " items in short-term memory."
      --, small <| "You made " ++ String.fromInt summary.totalMistakes ++ " mistakes in total."
      , Spil.videreButton sharedPlaying Videre OK
      ] ++ List.map (frameHisStr >> text) igang.frameHistory
      --|> text |> el [padding (s 6)] |> List.singleton

    Error string ->
      "ERROR from blink model " ++ string ++ ", msg not applicable"
      |> p |> el [padding (s 2)] |> List.singleton

  --billede "noegenhat" |> el [padding 40] |> List.singleton


view : Shared.Model -> Model -> View Msg
view shared model =
  { title = spilTitel "blink"
  , body = vis shared.playing model
  }
