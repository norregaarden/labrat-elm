module Pages.Spil.Count exposing (Model, Msg, page)

import Array exposing (fromList, get)
import Effect exposing (Effect)
import Element exposing (Element, alpha, centerX, column, el, fill, height, html, htmlAttribute, padding, row, spacing, text, width)
import Element.Border as Border
import Gen.Params.Spil.Count exposing (Params)
import Html.Events.Extra.Pointer as Pointer
import Page
import Random
import Random.Extra
import Random.List
import Request
import Set exposing (Set)
import Shared
import Svg exposing (svg)
import Svg.Attributes exposing (cx, cy, r, viewBox)
import UI exposing (appButton, h, s, small, spilTitel)
import UIColor exposing (green, red)
import View exposing (View)
import Page

-- SETTINGS

minNFrom = 2

numberOfBoards = 4

numberOfRounds = 5

-- Derived Settings

toN fromN = fromN + numberOfBoards - 1

minNTo = toN minNFrom

maxNTo = toN minNTo


-- PAGE

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }


-- MODEL / INIT


type Model
    = Intro
    | Igang IgangModel
    | Done  IgangModel
    | Error String


init : ( Model, Effect Msg )
init =
    ( Intro, Effect.none )

type alias IgangModel =
    { board : Board
    , clicking : Int
    , correct : Bool
    , mistakes : Mis
    , history : List His
    }

type alias Board =
    List (Set Int)

type alias Mis = Int

type alias His = (Mis, Board)

boardPlaceholder : Board
boardPlaceholder =
    -1
    |> List.singleton
    |> Set.fromList
    |> List.singleton

hisPlaceholder : His
hisPlaceholder = (-1, boardPlaceholder)

igangInit : IgangModel
igangInit =
    { board = []
    , clicking = -1
    , correct = False
    , mistakes = 0
    , history = []
    }

resetGameAndSaveHistory board igangModel =
    let
        newHistory =
            case igangModel.correct of
                False ->
                    igangModel.history
                True ->
                    (igangModel.mistakes, igangModel.board) :: igangModel.history
    in
    { igangModel
    | board = board
    , clicking = -1
    , correct = False
    , mistakes = 0
    , history = newHistory
    }

correctCount board =
    List.map Set.size board
    |> List.maximum
    |> Maybe.withDefault 0


-- UPDATE


type Msg
    = Begynd
    | Blandet Board
    | Click Int
    | Clicked


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        nextBoardOrFinish =
            let
                fromN =
                    case model of
                        Igang igangModel ->
                            minNFrom + List.length igangModel.history + 1
                        _ ->
                            minNFrom

                generate =
                    Effect.fromCmd <|
                    Random.generate (List.map (Tuple.first >> Set.fromList) >> Blandet) <|
                    Random.andThen Random.List.shuffle <|
                    Random.Extra.combine <|
                    List.map (\i -> Random.List.choices i (List.range 0 24)) (List.range fromN (toN fromN))

                historyLength =
                    fromN - minNFrom
            in
            -- nextBoard
            if historyLength < numberOfRounds then
                ( model
                , generate
                )
            -- OrFinish
            else
                case model of
                    Igang igangModel ->
                        ( Done (resetGameAndSaveHistory [] igangModel)
                        , Effect.none
                        )
                    _ ->
                        ( model, Effect.none )




    in
    case msg of
        Begynd ->
            nextBoardOrFinish

        Blandet blandet ->
            let
                oldIgang =
                    case model of
                        Igang igangModel ->
                            igangModel
                        _ ->
                            igangInit
            in
            ( Igang (resetGameAndSaveHistory blandet oldIgang)
            , Effect.none
            )

        Click i ->
            case model of
                Igang igangModel ->
                    let
                        actualCount =
                            Array.fromList igangModel.board
                            |> get i
                            |> Maybe.withDefault Set.empty
                            |> Set.size

                        correct =
                            (actualCount == correctCount igangModel.board)

                        misInt =
                            if correct then 0 else 1
                    in
                    ( Igang { igangModel
                        | clicking = i
                        , correct = correct
                        , mistakes = igangModel.mistakes + misInt
                        }
                    , Effect.none )

                _ -> ( model, Effect.none )

        Clicked ->
            case model of
                Igang igangModel ->
                    if igangModel.correct then
                        ( nextBoardOrFinish
                        )
                    else
                        ( model, Effect.none )
                _ -> ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
svgCircle n =
    let
        colors =
            Array.fromList ["#A65E2E", "#E55137", "#D36E70", "#57A639", "#F80000"
            , "#B32821", "#3D642D", "#C93C20", "#8A6642", "#84C3BE", "#6A5F31"
            , "#E4A010", "#9E9764", "#6D3F5B", "#424632", "#B8B799", "#35682D"]
        fill =
            colors
            |> get (modBy (Array.length colors) n)
            |> Maybe.withDefault "#112233"
    in
    html <| svg
        [ viewBox "0 0 100 100"
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        ]
        <| List.singleton <|
        Svg.circle
            [ cx "50", cy "50", r "50"
            , Svg.Attributes.fill fill ] []


visBoks : Set Int -> Element msg
visBoks numbers =
    let
        visDims n =
            el
                [ width fill
                , height fill
                , alpha (if Set.member n numbers then 1 else 0)
                ]
                <| svgCircle (Set.size numbers + n)

        boksRow r =
            row [ width fill ]
            <| List.map visDims (List.range (r*5) (r*5+4))
    in
    column [ width fill, height fill, padding (s 3) ]
    <| List.map boksRow (List.range 0 4)


fourSquares : List (Element Msg) -> Int -> Bool -> Element Msg
fourSquares list pressed correct =
    let
        greenBorder no =
            if no == pressed then
                [ if correct then Border.color green else Border.color red
                ]
            else []

        array = fromList list

        square no =
            Maybe.withDefault Element.none (get no array)
            |> el (greenBorder no ++
            [ Border.width 10, Border.rounded (s 3)
            , Pointer.onDown (\_ -> Click no) |> htmlAttribute
            , Pointer.onUp (\_ -> Clicked) |> htmlAttribute
            , width fill, height fill ])
    in
    column [ spacing (s 3) ]
        [ row [ spacing (s 3) ] [ square 0, square 1 ]
        , row [ spacing (s 3) ] [ square 2, square 3 ]
        ]





vis sharedPlaying model =
  let
    overskrift = h 2 "Can you count?"
  in
  case model of
    Intro ->
      [ overskrift
      , small <|
          "Click the largest set."
      , text ""
      , appButton Begynd "READY" |> el [centerX]
      ]

    Igang igangModel ->
        [ h 2 <| "Click the largest set (" ++ String.fromInt (correctCount igangModel.board) ++ " items)"
        , fourSquares (List.map visBoks igangModel.board) igangModel.clicking igangModel.correct
        ]

    Done igangModel ->
        let
            historyArray =
                Array.fromList igangModel.history
                |> Debug.log "historyArray"

            viewHistory i acc =
                let
                    (mis, board) =
                        get i historyArray
                        |> Maybe.withDefault hisPlaceholder

                    misStr =
                        String.fromInt mis ++ " mistakes."

                    itemsStr =
                        (correctCount board |> String.fromInt) ++ " items: "
                in
                (itemsStr ++ misStr |> text) :: acc

            --(\thisMany -> text <| String.fromInt thisMany ++ " items: ")
        in
        h 2 "play > count > stats" :: List.foldl viewHistory [] (List.range 0 (numberOfRounds - 1))

    Error error ->
        h 2 "play > count > error" :: h 3 error :: []

view : Shared.Model -> Model -> View Msg
view shared model =
  { title = spilTitel "count"
  , body = vis shared.playing model
  }
