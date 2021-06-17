module Pages.Spil.Dut exposing (Model, Msg, page)

import Dutter exposing (..)
import Element exposing (centerX, column, el, html, layout, padding, paragraph, pointer, spacing, text, wrappedRow)
import Element.Events exposing (onClick)
import Element.Input exposing (button)
import Gen.Params.Dut exposing (Params)
import Page
import Random as R
import Random.List exposing (choose, shuffle)
import Request
import Shared
import Task
import Time
import UI exposing (appButton)
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
    { udvalgtDut : Dut
    , dutterne : List Dut
    , tidspunkt : Int
    , klikket : List ( Bool, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { udvalgtDut = enDut
      , dutterne = []
      , tidspunkt = 0
      , klikket = []
      }
    , Cmd.none
    )



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
            ( model, bland )

        Blandet liste ->
            ( { model | dutterne = liste }, udvælg )

        Udvælg whatevs ->
            ( { model | udvalgtDut = fixDut whatevs }, Task.perform Start Time.now )

        Start tid ->
            ( { model | tidspunkt = Time.posixToMillis tid }, Cmd.none )

        Klik dut ->
            if dut == model.udvalgtDut then
                ( model, Task.perform (Gem True) Time.now )

            else
                ( model, Task.perform (Gem False) Time.now )

        Gem bål tid ->
            ( { model | klikket = ( bål, Time.posixToMillis tid - model.tidspunkt ) :: model.klikket }, bland )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


unit =
    8


størrelse =
    70


huskDutten dut =
    svgDut størrelse dut |> html |> el [ onClick (Klik dut) ]


view : Model -> View Msg
view model =
    { title = "Spil: Dut"
    , body =
        UI.appLayout <|
           column [ padding (unit * 4), spacing (unit * 2), centerX ] <|
                List.map (\e -> el [ centerX ] e)
                    [ html <| svgDut (størrelse * 2) model.udvalgtDut
                    , appButton Begynd "BEGYND"
                    , wrappedRow [ spacing (unit * 2) ] (List.map huskDutten model.dutterne)
                    , paragraph [] [ Debug.toString model.tidspunkt |> text ]
                    , paragraph [] [ Debug.toString model.klikket |> text ]
                    ]
    }

                   -- , text "Test af reaktionstid:"
                   --  , text "Klik på den matchende figur"