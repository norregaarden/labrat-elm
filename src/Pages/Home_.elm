module Pages.Home_ exposing (view)

-- elm-spa server

import UIColor exposing (blue, orangeLight)
import Element exposing (Element, column, el, fill, height, image, link, newTabLink, paddingXY, px, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Region exposing (heading)
import UI exposing (bltr, h, s, small)
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
  [ h 2 "Welcome, lab rat."
  , image [width (px 301), height (px 363), paddingXY 0 (s 3) ]
    { src = "/images/rottefyr.webp"
    , description = "rottefyr" }
  , text ""
  , newTabLink [Font.color blue] { url = "https://youtu.be/P9_LxDqkaZM", label = text "What is this? Click here for a video." }
  , text ""
  , text ""
  , el
      [heading 1, Font.color orangeLight, Font.extraBold, Font.size (s 7), paddingXY 0 (s 3), width fill, Border.widthEach (bltr 0 0 1 0)]
      ( column [spacing (s 1)] [text "lab rat", small "the psychonaut tracker app"] )
  , newTabLink [Font.color blue] { url = "https://reddit.com/r/labratapp", label = text "got feedback? r/labratapp" }
  , small "version 0.1.3"
  , small "updated 22-01-31"
  , newTabLink [Font.color blue] { url = "https://github.com/norregaarden/labrat-elm", label = text "view source on github" }
  ]


view : View msg
view =
  { title = "lab rat"
  , body = vis
  }