module Pages.Home_ exposing (view)

-- elm-spa server

import Element exposing (Element, column, el, layout, link, padding, spacing, text)
import Element.Font as Font
import UI
import View exposing (View)

-- MODEL

type alias Model =
    {}

init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE

type Msg = Intet



-- VIEW

vis: Model -> Element msg
vis model =
    column [spacing 20, padding 20, Font.size 30]
    [ text "Prøv et spil:"
    , link [] {url = "/spil/dut", label = text "dut"}
    , link [] {url = "/spil/tid", label = text "tid"}
    , link [] {url = "/spil/blink", label = text "blink"}
    ]

view : View msg
view =
    { title = "Cognitive Lab Rat"
    --, body = [ Html.text "Hello, world!" ]
    , body = vis {} |> UI.appLayout
    }