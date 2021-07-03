module Pages.Home_ exposing (view)

-- elm-spa server

import Element exposing (Element, fill, image, paddingXY, text, width)
import UI exposing (h, s, small)
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
  [ h 1 "Welcome, lab rat."
  , image [width fill, paddingXY 0 (s 3) ]
    { src = "/images/rottefyr.png"
    , description = "rottefyr" }
  , text ""
  , small "lab rat"
  , small "v 0.1"
  ]


view : View msg
view =
  { title = "lab rat"
  , body = vis
  }