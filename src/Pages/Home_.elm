module Pages.Home_ exposing (view)

-- elm-spa server

import Element exposing (Element, el, link, pointer, text)
import Element.Font as Font
import UI exposing (s)
import View exposing (View)
import Gen.Route as Route exposing (Route)


-- MODEL

type alias Model =
    {}

init : ( Model, Cmd msg )
init =
    ( {}, Cmd.none )


-- VIEW

vis: List (Element msg)
vis =
    el [Font.size (s 1)] (text "Try a game :") ::
    List.map (\l -> link [Font.size (s 3)] l)
        [ {url = Route.toHref Route.Spil__Dut, label = text "dut"}
        , {url = Route.toHref Route.Spil__Tid, label = text "tid"}
        , {url = Route.toHref Route.Spil__Blink, label = text "blink"} ]


view : View msg
view =
    { title = "lab rat - cognitive tests for humans"
    , body = vis |> UI.appLayout
    }