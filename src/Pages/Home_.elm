module Pages.Home_ exposing (view)

-- elm-spa server

import Element exposing (Element)
import UI exposing (h)
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
  h 1 "Welcome, lab rat." |> List.singleton


view : View msg
view =
  { title = "lab rat - cognitive tests for humans"
  , body = vis
  }