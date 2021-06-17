module Pages.Spil.Blink exposing (Model, Msg, page)

import Element exposing (Element, el, fill, image, layout, padding, width)
import Gen.Params.Blink exposing (Params)
import Page
import Request
import Shared
import UI
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
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

vis : Model -> Element Msg
vis model =
    billede "noegenhat" |> el [padding 40]


view : Model -> View Msg
view model =
    { title = "Spil: Blink"
    , body = vis model |> UI.appLayout
    }



-- VIEW helpers

billede : String -> Element msg
billede navn =
    image [width fill] { src = "/images/" ++ navn ++ ".svg", description = "" }