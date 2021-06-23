module Pages.Spil.Dut exposing (Model, Msg, page)

import Dutter exposing (..)
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

runder = 10


-- INIT

type alias Model =
    { udvalgtDut : Dut
    , dutterne : List Dut
    , igang : Bool
    , tidspunkt : Int
    , klikket : List ( Bool, Int )
    , færdig : Bool
    }



init : ( Model, Cmd Msg )
init =
    ( { udvalgtDut = enDut
      , dutterne = []
      , igang = False
      , tidspunkt = 0
      , klikket = []
      , færdig = False
      }
    , Cmd.none
    )


-- PAGE

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }




-- UPDATE

type Msg
    = Begynd
    | Blandet (List Dut)
    | Udvælg ( Maybe Dut, List Dut )
    | Start Time.Posix
    | Klik Dut
    | Gem Bool Time.Posix

fixDut : ( Maybe Dut, List Dut ) -> Dut
fixDut ( maybeDut, _ ) =
    case maybeDut of
        Nothing ->
            enDut

        Just dut ->
            dut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        bland =
            R.generate Blandet (shuffle alleDutter)

        udvælg =
            R.generate Udvælg (choose alleDutter)
    in
    case msg of
        Begynd ->
            ( { model | igang = True, færdig = False}, bland )

        Blandet liste ->
            ( { model | dutterne = liste }, udvælg )

        Udvælg whatevs ->
            ( { model | udvalgtDut = fixDut whatevs }, Task.perform Start Time.now )

        Start tid ->
            ( { model | tidspunkt = Time.posixToMillis tid }, Cmd.none )

        Klik dut ->
            if dut == model.udvalgtDut then
                ( model, Task.perform (Gem True) Time.now )
            else ( model, Task.perform (Gem False) Time.now )

        Gem bål tid ->
            let
                deltaTid = Time.posixToMillis tid - model.tidspunkt
                nyKlikket = ( bål, deltaTid ) :: model.klikket
                alleRunder = (List.length nyKlikket == runder)
            in
            ( { model | klikket = nyKlikket, færdig = alleRunder, igang = (not alleRunder) }, bland )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> View Msg
view model =
    { title = spilTitel "Shapes and colors"
    , body =
        List.map (\e -> el [ centerX ] e)
            [ topView model
            , buttonView model
            , dutterView model
            ]
        ++ [dataView model]
    }

topView model =
    if model.igang then
        svgDut model.udvalgtDut |> html
        |> el [width (maximum (s 11) fill), paddingEach (bltr (s 3) (s 3) 0 (s 3))]
        |> el [Border.widthEach (bltr 2 0 0 0)]
    else
        column []
            [ p "Test your reaction time" |> el [Font.size (s 3)]
            , column [padding (s 1), spacing (s 1), Font.size (s 1)]
                [ p "Match shape and color." |> el []
                , p (fromInt runder ++ " rounds.") |> el []
                ]
            ]

buttonView model =
    showWhen (not model.igang && not model.færdig) (appButton Begynd "BEGIN")

dataView model =
    let
        klikSeconds k = (second k |> toFloat) / 1000

        correct = List.length (List.filter (first >> (==) True) model.klikket)

        mean = List.sum (List.map (klikSeconds) model.klikket) / runder
        meanString = Round.round 3 mean

        variance =
            if runder > 1 then
                List.sum (List.map (\k -> (klikSeconds k - mean)^2) model.klikket) / (runder - 1)
            else 0
        spread = sqrt variance
        spreadString = Round.round 3 spread
    in
        [ row [width fill, spaceEvenly]
            [ "Correct :" |> text |> el [width (fillPortion 1)]
            , fromInt correct ++ " / " ++ fromInt runder |> text |> el [width (fillPortion 1), Font.alignRight]
            ]
        , row [width fill, spaceEvenly]
            [ "Average :" |> text |> el [width (fillPortion 1)]
            , meanString ++ " s" |> text |> el [width (fillPortion 1), Font.alignRight]
            ]
        , row [width fill, spaceEvenly]
            [ "Spread  :" |> text |> el [width (fillPortion 1)]
            , spreadString ++ " s" |> text |> el [width (fillPortion 1), Font.alignRight]
            ]
        --, p (Debug.toString model.klikket)
        ]
        |> column [padding (s 3), spacing (s 3), width fill]
        |> showWhen model.færdig

dutterView model =
    let
        dutterRow dutter =
            List.map (el [width (fillPortion 1)]) dutter |> row [spacing (s 2), width fill, height fill]

        dutterWrapped dutter =
            if List.length dutter > 0 then
                dutterRow (List.take 4 dutter) :: dutterWrapped (List.drop 4 dutter)
            else []
    in
        --p (Debug.toString <| dutterWrapped (List.map huskDutten model.dutterne))
        dutterWrapped (List.map huskDutten model.dutterne)
        |> column [ spacing (s 2), paddingEach (bltr (s 2) (s 2) 0 (s 2)), width fill, height fill ]
        |> showWhen model.igang

huskDutten dut =
    svgDut dut |> html |> el [ onClick (Klik dut) ]


-- HELPERS

boolToInt bool = if bool then 1 else 0