module Pages.Spil.Tid exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (Element, centerX, column, el, fill, padding, paddingXY, spacing, width)
import Element.Font as Font
import Gen.Params.Tid exposing (Params)
import Page
import Request exposing (Request)
import Round
import Shared
import Spil exposing (Score(..))
import String exposing (fromFloat, fromInt)
import Task
import Time
import UI exposing (appButton, p, s, showListWhen, showWhen, spilTitel)
import View exposing (View)

-- SETTINGS

burde : Int
burde = 10
  * 1000 -- ms


-- MODEL

type alias Model =
  { start : Int
  , slut : Int
  , igang : Bool
  , færdig : Bool
  }


init : ( Model, Effect Msg )
init =
  ( Model 0 0 False False
  , Effect.none )


-- BOILERPLATE

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
  Page.advanced
    { init = init
    , update = update
    , view = view shared
    , subscriptions = \_ -> Sub.none
    }


-- UPDATE

type Msg
    = Startklik
    | Start Time.Posix
    | Slutklik
    | Slut Time.Posix
    | Videre
    | Nå


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
  case msg of
    Startklik ->
      ( { model | igang = True, færdig = False }
      , Task.perform Start Time.now |> Effect.fromCmd )

    Start tid ->
      ( { model | start = Time.posixToMillis tid }
      , Cmd.none |> Effect.fromCmd )

    Slutklik ->
      ( { model | igang = False }
      , Task.perform Slut Time.now |> Effect.fromCmd )

    Slut tid ->
      ( { model | slut = Time.posixToMillis tid, færdig = True }
      , Effect.none
      )

    Videre ->
      ( model
      , score model |> TidScore |> Shared.SpilScore |> Effect.fromShared
      )

    Nå ->
      ( model
      , Shared.GoToPlay |> Effect.fromShared
      )


score model =
  { burde = burde
  , faktisk = faktisk model
  }



-- HELPERS

faktisk : Model -> Int
faktisk model =
  model.slut - model.start

sekunder ms =
  toFloat ms / 1000



-- VIEW

vis sharedPlaying model =
  let
    indhold =
      if (not model.igang) then
        [ p "Press START when you're ready."
        , (appButton Startklik "START") |> el [centerX, padding (s 4)]
        ]
      else
        [ p ("Press STOP when you believe " ++ fromFloat (sekunder burde) ++ " seconds have passed")
        , (appButton Slutklik "STOP") |> el [] |> el [centerX, padding (s 1)]
        ]

    noget =
      [ "How long is " ++ fromFloat (sekunder burde) ++" seconds?" |> p |> el [Font.size (s 3)]
      , column [padding (s 1), spacing (s 1), Font.size (s 1)] indhold |> showWhen (not model.færdig)
      ]

    nogetmere =
      [column [spacing (s 1), paddingXY 0 (s 4)]
        [ Round.round 3 egentlig ++ " seconds passed." |> p
        , "You were off by " |> p
        , Round.round 3 forbi ++ " seconds." |> p
        , Spil.videreButton sharedPlaying Videre Nå
        ]
      ]
      |> List.map (el [width fill])
      |> showListWhen model.færdig

    egentlig = sekunder (faktisk model)
    forbi = egentlig - (sekunder burde)
  in
  (noget ++ nogetmere)
  |> List.map (el [centerX])


view : Shared.Model -> Model -> View Msg
view shared model =
  { title = spilTitel (fromInt burde ++ " seconds")
  , body = vis shared.playing model
  }
