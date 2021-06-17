module Pages.Spil.Tid exposing (Model, Msg, page)

import Element exposing (Element, column, el, layout, padding, paddingXY, paragraph, pointer, spacing, text)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Tid exposing (Params)
import Page
import Request
import Shared
import String exposing (fromFloat, fromInt)
import Task
import Time
import UI exposing (appButton, opacityFromBool, p)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { burde : Int
    , start : Int
    , slut : Int
    , igang : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 10 0 0 False, Cmd.none )



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
            ( { model | igang = True }, Task.perform Start Time.now )

        Start tid ->
            ( { model | start = Time.posixToMillis tid }, Cmd.none )

        Slutklik ->
            ( { model | igang = False }, Task.perform Slut Time.now )

        Slut tid ->
            ( { model | slut = Time.posixToMillis tid }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

vis model =
    let
        indhold =
            if (not model.igang) then
                [ text "Tryk START når du er klar."
                , text ""
                , (appButton Startklik "START") -- el [ onClick Startklik, pointer ]
                ]
            else
                [ "Tryk STOP når du tror der er gået " ++ fromInt model.burde ++ " sekunder" |> p
                , (appButton Slutklik "STOP") -- el [ onClick Slutklik, pointer ]
                ]

        noget =
            [ text <| "Hvor lang tid er " ++ fromInt model.burde ++" sekunder?"
            , column [padding 16, spacing 16] indhold
            ]

    in
    noget ++
         [ text ""
         , difference model |> fromInt |> text
         , text ""

         --, fromInt model.start |> text
         --, fromInt model.slut |> text
         ]
         |> List.map (el [])
         |> column [ paddingXY 32 64, spacing 32 ]


view : Model -> View Msg
view model =
    { title = "Spil - Tid | lab rat"
    , body = vis model |> UI.appLayout
    }



-- VIEW helpers

difference : Model -> Int
difference model =
    let diff = model.slut - model.start in
    if diff > 0 then diff else 0