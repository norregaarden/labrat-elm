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
import String exposing (toInt)
import Task
import Time
import UI exposing (appButton, h, p, s, showListWhen, showWhen, small, spilTitel)
import View exposing (View)


blinkTimes =
    2


-- MODEL


type alias Model =
    { status : ModelStatus
    , targetDuration : Float --ms
    , frames : List String
    , frame : Frame
    , frameFuture : List String
    , frameHistory : List Frame
    }

type ModelStatus
    = Intro
    | Blinking
    | Choose
    | Done
    | Error String

type alias Frame =
    { string : String
    , deltas : List Float
    }


modelPlaceholder status =
    { status = status
    , targetDuration = 120
    , frames = []
    , frame = Frame "" []
    , frameFuture = []
    , frameHistory = []
    }


init : ( Model, Effect Msg )
init =
    ( modelPlaceholder Intro
    , Effect.none
    )



-- BOILERPLATE


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = \_ -> onAnimationFrameDelta OnFrame
        }



-- UPDATE


type Msg
    = Begin
    | Shuffled (List String)
    | OnFrame Float
    | Videre
    | OK


{-toJson : IgangModel -> E.Value
toJson igang =
    E.object
        [ ( "duration", E.int igang.duration )
        , ( "jsImage"
          , E.object
                [ ( "src", E.string igang.jsImage.src )
                , ( "description", E.string igang.jsImage.description )
                ]
          )
        ]-}


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        random =
            Effect.fromCmd <|
                Random.generate (List.intersperse "" >> Shuffled) (Random.List.shuffle ["A", "B", "C", "D"])
    in
    case msg of
        Begin ->
            ( model
            , random )

        Shuffled list ->
            let
                frames =
                    list ++ ["", "", ""] ++ list ++ [""]
            in
            ( { model
            | status = Blinking
            , frames = frames
            , frameFuture = frames
            }
            , Effect.none )

        OnFrame delta ->
            case model.status of
                Blinking ->
                    let
                        {-(status, frame, future) =
                            case model.frameFuture of
                                [] ->
                                    (Done, "end", [])
                                f :: fs ->
                                    (Blinking, f, fs)

                        fuck =
                            List.range 0 1000000-}
                        oldFrame =
                            model.frame

                        oldDeltas =
                            oldFrame.deltas

                        newDeltas =
                            delta :: oldDeltas

                        updatedFrame =
                            { oldFrame
                            | deltas = newDeltas
                            }

                        deltaSum =
                            List.sum newDeltas

                        changeFrame =
                            deltaSum > model.targetDuration

                        (newFrame, newFuture, newHistory) =
                            if changeFrame then
                                case model.frameFuture of
                                    [] ->
                                        (Frame "" []
                                        , model.frameFuture
                                        , model.frameHistory
                                        )
                                    f :: fs ->
                                        (Frame f []
                                        , fs
                                        , updatedFrame :: model.frameHistory
                                        )
                            else
                                ( updatedFrame
                                , model.frameFuture
                                , model.frameHistory )

                        nextRound =
                            changeFrame && List.length model.frameFuture == 0

                        newTargetDuration =
                            if nextRound then
                                model.targetDuration / 2
                            else model.targetDuration
                    in
                    ( { model
                    | status = if nextRound then Choose else Blinking --if newTargetDuration < 15 then Done else Blinking
                    , targetDuration = newTargetDuration
                    , frame = newFrame
                    , frameFuture = newFuture
                    , frameHistory = newHistory
                    }
                    , Effect.none )
                    --, if List.length model.frameFuture == 0 then random else Effect.none )

                _ ->
                    ( model, Effect.none )

        Videre ->
            ( model
            , Effect.none
            --, score model |> BlinkScore |> Shared.SpilScore |> Effect.fromShared
            )

        OK ->
            ( model
            , Shared.GoToPlay |> Effect.fromShared
            )

{-    case model of
        Intro ->
            case msg of
                Begin ->
                    ( Igang igangModelPlaceholder
                    , randomImage
                    )

                _ ->
                    ( Error "Intro", Effect.none )

        Igang igang ->
            case msg of
                Shuffled ( maybeImage, _ ) ->
                    ( Igang
                        { igang
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
                                    [ ( "", time ) ]

                                ( oldStr, oldTime ) :: fs ->
                                    ( newStr, time ) :: igang.frameHistory

                        {- if newStr /= oldStr then
                             (newStr, time) :: igang.frameHistory
                           else
                             igang.frameHistory
                        -}
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
            ( Error (string ++ " then Error"), Effect.none )-}




{-score model =
    case model of
        Igang igangModel ->
            { expectedDuration_ms = igangModel.duration
            , realDuration_ms = igangModel.duration
            }

        _ ->
            { expectedDuration_ms = -1
            , realDuration_ms = -1
            }-}



-- HELPERS
-- VIEW


viewBlinking : Model -> List (Element msg)
viewBlinking model =
    let
        content =
            text model.frame.string
                |> el [ centerX, centerY, Font.size (s 11), paddingXY 0 (s 5), Font.extraBold ]
    in
    [ content ]

viewChoose : Model -> List (Element msg)
viewChoose model =
    let
        content =
            h 2 (Debug.toString model.frameHistory)
                |> el [ centerX, centerY ]
    in
    [ content ]


vis sharedPlaying model =
    let
        titelskrift =
            el [ Font.extraBold ] <| text "blink"

        overskrift =
            h 2 "Find your minimal recognition time"
    in
    case model.status of
        Intro ->
            [ titelskrift
            , overskrift
            , small <|
                "An image will blink "
                    ++ String.fromInt blinkTimes
                    ++ " times for a short amount of time."
            , small <|
                "Afterwards, click the corresponding image."
            , text ""
            , appButton Begin "READY" |> el [ centerX ]
            ]

        Blinking ->
            viewBlinking model

        Choose ->
            viewChoose model

        Done ->
            let
                frameHisStr frame =
                    frame.string ++ ": "
                        ++ Debug.toString frame.deltas
            in
            [ overskrift
            , Spil.videreButton sharedPlaying Videre OK
            ]
                ++ List.map (frameHisStr >> text) model.frameHistory

        Error string ->
            "ERROR from blink model "
                ++ string
                ++ ", msg not applicable"
                |> p
                |> el [ padding (s 2) ]
                |> List.singleton


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = spilTitel "blink"
    , body = vis shared.playing model
    }
