module Pages.Spil.Count exposing (Model, Msg, page)

import Array exposing (fromList, get)
import Effect exposing (Effect)
import Element exposing (Element, alpha, centerX, column, el, fill, height, html, htmlAttribute, padding, row, spacing, text, width)
import Element.Border as Border
--import Element.Events as Events
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



page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Intro
    | Igang IgangModel


init : ( Model, Effect Msg )
init =
    ( Intro, Effect.none )

type alias IgangModel =
    { blandet : Board
    , clicking : Int
    , correct : Bool
    , history : List (Bool, Board)
    }

type alias Board =
    List (Set Int)

igangInit : IgangModel
igangInit =
    { blandet = []
    , clicking = -1
    , correct = False
    , history = []
    }

newgame blandet igang =
    { igang
    | blandet = blandet
    , clicking = -1
    , correct = False
    }

correctCount igang =
    List.map Set.size igang.blandet
    --|> List.sort
    --|> List.drop (List.length igang.clicked)
    |> List.maximum
    |> Maybe.withDefault 0


-- UPDATE


type Msg
    = Begynd
    | Blandet (List (Set Int))
    | Click Int
    | Clicked


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        bland n =
            Effect.fromCmd <|
            Random.generate (List.map (Tuple.first >> Set.fromList) >> Blandet) <|
            Random.andThen Random.List.shuffle <|
            Random.Extra.combine <|
            List.map (\i -> Random.List.choices i (List.range 0 24)) (List.range n (n+3))

    in
    case msg of
        Begynd ->
            let
                fra =
                    case model of
                        Intro ->
                            3
                        Igang igangModel ->
                            3 + List.length igangModel.history
            in
            ( model
            , bland fra
            )

        Blandet blandet ->
            ( Igang { igangInit | blandet = blandet }
            , Effect.none
            )

        Click no ->
            case model of
                Igang igangModel ->
                    let
                        actualCount =
                            Array.fromList igangModel.blandet
                            |> get no
                            |> Maybe.withDefault Set.empty
                            |> Set.size
                    in
                    ( Igang { igangModel
                        | clicking = no
                        , correct = (actualCount == correctCount igangModel)
                        }
                    , Effect.none )

                _ -> ( model, Effect.none )

        Clicked ->
            case model of
                Igang igangModel ->
                    if igangModel.correct then
                        ( Igang { igangModel
                            | clicking = -1
                            --, clicked = igangModel.clicking :: igangModel.clicked
                            , blandet = List.drop 1 igangModel.blandet
                        }, Effect.none )
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
        -- Element.explain Debug.todo
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
      --, small <| String.fromInt startHuskNumber ++ " images in the first round."
      , text ""
      , appButton Begynd "READY" |> el [centerX]
      --, viewImages Husk.allImages False
      ]

    Igang igangModel ->
        let
            im = Debug.log "hej" igangModel
        in
        [ h 2 <| "Click the largest set (" ++ String.fromInt (correctCount igangModel) ++ " items)"
        , fourSquares (List.map visBoks igangModel.blandet) igangModel.clicking igangModel.correct
        ]

view : Shared.Model -> Model -> View Msg
view shared model =
  { title = spilTitel "count"
  , body = vis shared.playing model
  }
