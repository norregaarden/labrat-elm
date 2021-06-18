module Pages.Home_ exposing (view)

-- elm-spa server

import Element exposing (Element, el, link, text)
import Element.Font as Font
import UI exposing (s)
import View exposing (View)


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
        [ {url = "/spil/dut", label = text "dut"}
        , {url = "/spil/tid", label = text "tid"}
        , {url = "/spil/blink", label = text "blink"} ]


view : View msg
view =
    { title = "lab rat - cognitive tests for humans"
    , body = vis |> UI.appLayout
    }