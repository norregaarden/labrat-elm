module Pages.Spil.Tid exposing (Model, Msg, page)

import Element exposing (Element, centerX, column, el, fill, padding, paddingXY, spacing, text, width)
import Element.Font as Font
import Gen.Params.Tid exposing (Params)
import Page
import Request
import Round
import Shared
import String exposing (fromInt)
import Task
import Time
import UI exposing (appButton, p, s, showListWhen, showWhen, spilTitel)
import View exposing (View)

-- SETTINGS

burde = 10 -- sekunder


-- MODEL

type alias Model =
    { start : Int
    , slut : Int
    , igang : Bool
    , færdig : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 False False, Cmd.none )


-- BOILERPLATE

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
    = Startklik
    | Start Time.Posix
    | Slutklik
    | Slut Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Startklik ->
            ( { model | igang = True, færdig = False }, Task.perform Start Time.now )

        Start tid ->
            ( { model | start = Time.posixToMillis tid }, Cmd.none )

        Slutklik ->
            ( { model | igang = False }, Task.perform Slut Time.now )

        Slut tid ->
            ( { model | slut = Time.posixToMillis tid, færdig = True }, Cmd.none )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

vis model =
    let
        indhold =
            if (not model.igang) then
                [ p "Press START when you're ready."
                , (appButton Startklik "START") |> el [centerX, padding (s 4)]
                ]
            else
                [ p ("Press STOP when you believe " ++ fromInt burde ++ " seconds have passed")
                , (appButton Slutklik "STOP") |> el [] |> el [centerX, padding (s 1)]
                ]

        noget =
            [ "How long is " ++ fromInt burde ++" seconds?" |> p |> el [Font.size (s 3)]
            , column [padding (s 1), spacing (s 1), Font.size (s 1), width fill] indhold
            ]

        sekunder = (difference model |> toFloat)/1000
        forbi = sekunder - burde


    in
    noget ++ List.singleton (column [spacing (s 1), paddingXY (s 1) (s 4)]
         ([ Round.round 3 sekunder ++ " seconds passed."
         , "You were off by "
         , Round.round 3 forbi ++ " seconds."
         ]
         |> List.map (p)
         |> List.map (el [width fill])
         |> showListWhen model.færdig))


view : Model -> View Msg
view model =
    { title = spilTitel (fromInt burde ++ " seconds")
    , body = vis model |> UI.appLayout
    }



-- VIEW helpers

difference : Model -> Int
difference model =
    let diff = model.slut - model.start in
    if diff > 0 then diff else 0