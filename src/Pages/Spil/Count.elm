module Pages.Spil.Count exposing (Model, Msg, page)

import Array exposing (fromList, get)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, alpha, centerX, column, el, fill, fillPortion, height, html, padding, row, spacing, text, width)
import Gen.Params.Spil.Count exposing (Params)
import Page
import Random
import Random.Extra
import Random.List
import Request
import Set exposing (Set)
import Shared
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, r, viewBox)
import UI exposing (appButton, h, s, small, spilTitel)
import View exposing (View)
import Page



page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
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
    { blandet : List (Set Int)

    }

igangInit =
    { blandet = [] }


-- UPDATE


type Msg
    = Begynd
    | Blandet (List (Set Int))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    let
        bland n =
            Effect.fromCmd <|
            Random.generate (List.map (Tuple.first >> Set.fromList) >> Blandet) <|
            Random.Extra.combine <|
            List.map (\i -> Random.List.choices i (List.range 0 24)) (List.range n (n+3))

    in
    case msg of
        Begynd ->
            ( model
            , bland 4
            )

        Blandet blandet ->
            ( Igang { igangInit | blandet = blandet }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
svgCircle =
    html <| svg
        [ viewBox "0 0 100 100"
        , Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        ]
        <| List.singleton <|
        Svg.circle
            [ cx "50", cy "50", r "50"
            , Svg.Attributes.fill "#112233" ] []


visBoks : Set Int -> Element msg
visBoks numbers =
    let
        visDims n =
            el
                [ width fill
                , height fill
                , alpha (if Set.member n numbers then 1 else 0)
                ]
                <| svgCircle
        -- Element.explain Debug.todo
        boksRow r =
            row [ width fill ]
            <| List.map visDims (List.range (r*5) (r*5+4))
    in
    column [ width fill, height fill, padding (s 3) ]
    <| List.map boksRow (List.range 0 4)


fourSquares : List (Element msg) -> Element msg
fourSquares list =
    let
        array = fromList list
        square no = Maybe.withDefault Element.none (get no array)
    in
    column []
        [ row [] [ square 0, square 1 ]
        , row [] [ square 2, square 3 ]
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
        [ overskrift
        , fourSquares (List.map visBoks igangModel.blandet)
        ]

view : Shared.Model -> Model -> View Msg
view shared model =
  { title = spilTitel "count"
  , body = vis shared.playing model
  }
