module Pages.LogChoose exposing (Model, Msg, page)

import Element exposing (none)
import Gen.Route as Route exposing (Route)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import UI exposing (appButton)
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared request =
    Page.element
        { init = init
        , update = update request
        , view = view shared.storage
        , subscriptions = \_ -> Sub.none
        }

-- SUBSCRIPTIONS
{-
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
-}


-- MODEL


type alias Model
  = {}

init : ( Model, Cmd Msg )
init = ( {}, Cmd.none )

type alias LogRoute = (String, Route)

logRoutes : List LogRoute
logRoutes = [("TempC", Route.Log__TempC)]


-- UPDATE


type Msg
    = ChosenDataType LogRoute


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update request msg model =
  case msg of
    ChosenDataType ( _ , route ) -> ( model, Request.pushRoute route request )





-- VIEW

view : Storage -> Model -> View Msg
view storage model =
  let
    dtButton (t, r) =
      appButton (ChosenDataType (t, r)) t

    body =
      logRoutes |> List.map dtButton
  in
    { title = "log | lab rat"
    , body = body |> UI.appLayout
    }
