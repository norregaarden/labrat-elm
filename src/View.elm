module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (Element)
import Url exposing (Url)
import Gen.Route as Route
import UI exposing (..)


type alias View msg =
    { title : String
    , body : List (Element msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Element.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (Element.map fn) view.body
    }


toBrowserDocument : Url -> View msg -> Browser.Document msg
toBrowserDocument url view =
    let
      route = Route.fromUrl url
      log1 = Debug.log "her" route
      log2 = Debug.log "der" url
      headerRoute =
        if String.slice 1 4 url.path == "log" then
          Route.LogChoose
        else if String.slice 1 5 url.path == "data" then
          Route.Data
        else if String.slice 1 5 url.path == "play" then
          Route.Play
        else if String.slice 1 5 url.path == "spil" then
          Route.Play
        else
          Route.Home_
    in
    { title = view.title
    , body = UI.appLayout headerRoute view.body
    }
