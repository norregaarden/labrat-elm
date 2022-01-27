module Pages.Log.Drug exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (centerX, column, el, fill, fillPortion, height, htmlAttribute, maximum, padding, paddingEach, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Log.Drug exposing (Params)
import Html.Attributes exposing (type_)
import Page
import Process
import Storage exposing (Storage)
import Task
import Request
import Shared
import Time
import UI exposing (appButton, bltr, h, p, s)
import UIColor exposing (blue, orangeLight, white)
import View exposing (View)
import Page
import Log exposing (Data(..), Weight(..), WeightUnit(..))

import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import RemoteData exposing (RemoteData(..))
import PSwiki.Object.Substance as Substance
import PSwiki.Object.SubstanceImage as SubstanceImage
import PSwiki.Query as Query

import Dropdown
import Helpers.DropdownConfig exposing (dropdownConfig)



page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
  Page.advanced
    { init = init
    , update = update shared.storage
    , view = view
    , subscriptions = \_ -> Sub.none
    }

-- INIT


type alias Model =
  { graphql : GraphQLModel
  , input : String
  , drug : Maybe Drug
  , searchError : String
  , roa : Dropdown
  , weightChoose : ChooseWeight
  , weightQuan : { unit : Dropdown, amount : Int }
  , weightQual : Dropdown
  , minutesAgo : Int
  }

type alias Drug =
  { name : String
  , url : String
  , image : Maybe String
  }

type alias Dropdown =
  { state : Dropdown.State String
  , selectedOption : Maybe String
  }

type ChooseWeight
  = Quantitative
  | Qualitative

init : ( Model, Effect Msg )
init =
  ( { graphql = RemoteData.Loading
    , input = ""
    , drug = Nothing
    , searchError = "hint: hyphens dont matter"
    , roa = { state = Dropdown.init "dropdown_roa", selectedOption = Nothing }
    , weightChoose = Quantitative
    , weightQuan =
      { unit =
        { state = Dropdown.init "dropdown_wu"
        , selectedOption = Just (Log.text_wu Milligram) }
      , amount = 0 }
    , weightQual = { state = Dropdown.init "dropdown_dq", selectedOption = Nothing }
    , minutesAgo = 0
    }
  , Effect.none
  )

dd_roa =
  dropdownConfig "Select ROA" (List.map Log.text_roa Log.allROAs) ROA_DropdownMsg ROA_Picked

dd_wu =
  dropdownConfig "unit" (List.map Log.text_wu Log.allWUs) WU_DropdownMsg WU_Picked

dd_dq =
  dropdownConfig "choose qualifier" (List.map Log.text_dq Log.allDQs) DQ_DropdownMsg DQ_Picked


-- UPDATE


type Msg
    = GotResponse GraphQLModel
    | ChangedInput String
    | Delayed String
    | ROA_DropdownMsg (Dropdown.Msg String)
    | ROA_Picked (Maybe String)
    | WeightChoose ChooseWeight
    | WU_DropdownMsg (Dropdown.Msg String)
    | WU_Picked (Maybe String)
    | WeightQuanChanged String
    | DQ_DropdownMsg (Dropdown.Msg String)
    | DQ_Picked (Maybe String)
    | MinutesAgoChanged String
    | Save
    | LogDataTid Time.Posix

delay : Float -> String -> (String -> Msg) -> Cmd Msg
delay time input msg =
  Process.sleep time
  |> Task.andThen (always <| Task.succeed (msg input))
  |> Task.perform identity


update : Storage -> Msg -> Model -> ( Model, Effect Msg )
update storage msg model =
  case msg of
    GotResponse response ->
      ( { model | graphql = response
        , drug = drugData response
        , searchError = "no results for '" ++ model.input ++ "'"
        }
      , Effect.none )
    ChangedInput input ->
      let
        drug =
          if String.trim input == "" then
            Nothing
          else model.drug

        error =
          if String.trim input == "" then
            "search for your drug"
          else
            "loading '" ++ input ++ "'"
      in
      ( { model | input = input, drug = drug, searchError = error }
      , delay 666 input Delayed |> Effect.fromCmd )

    Delayed input ->
      let
        cmd =
          if input == model.input then
            makeRequest input
          else
            Cmd.none
      in
      ( model, cmd |> Effect.fromCmd )

    ROA_DropdownMsg dd_msg ->
      let
        roa = model.roa
        ( state, cmd ) =
          Dropdown.update dd_roa dd_msg model.roa model.roa.state
        new_roa = { roa | state = state }
      in
      ( { model | roa = new_roa }
      , Effect.fromCmd cmd )

    ROA_Picked option ->
      let
        roa = model.roa
        new_roa = { roa | selectedOption = option }
      in
      ( { model | roa = new_roa }, Effect.none )

    WeightChoose qua ->
      ( { model | weightChoose = qua }, Effect.none )

    WU_DropdownMsg dd_msg ->
      let
        w = model.weightQuan
        wu = w.unit
        ( state, cmd ) =
          Dropdown.update dd_wu dd_msg model.weightQuan.unit model.weightQuan.unit.state
        new_wu = { wu | state = state }
        new_w = { w | unit = new_wu }
      in
      ( { model | weightQuan = new_w }
      , Effect.fromCmd cmd )

    WeightQuanChanged input ->
      let
        w = model.weightQuan
        new_w = { w | amount = Maybe.withDefault 0 (String.toInt input) }
      in
      ( { model | weightQuan = new_w }
      , Effect.none )

    WU_Picked option ->
      let
        w = model.weightQuan
        wu = w.unit
        new_wu = { wu | selectedOption = option }
        new_w = { w | unit = new_wu }
      in
      ( { model | weightQuan = new_w }, Effect.none )

    DQ_DropdownMsg dd_msg ->
      let
        dq = model.weightQual
        ( state, cmd ) =
          Dropdown.update dd_dq dd_msg model.weightQual model.weightQual.state
        new_dq = { dq | state = state }
      in
      ( { model | weightQual = new_dq }
      , Effect.fromCmd cmd )

    DQ_Picked option ->
      let
        dq = model.weightQual
        new_dq = { dq | selectedOption = option }
      in
      ( { model | weightQual = new_dq }, Effect.none )

    MinutesAgoChanged input ->
      ( { model | minutesAgo = Maybe.withDefault 0 (String.toInt input) }, Effect.none )

    Save ->
      ( model, Task.perform LogDataTid Time.now |> Effect.fromCmd )

    LogDataTid tid ->
      let
        log_time =
          Time.posixToMillis tid

        data_time =
          log_time - Log.millisFromMinutes model.minutesAgo

        drug =
          case model.drug of
            Nothing -> "ERROR"
            Just d -> d.name

        roa =
          Log.roa_from <|
            Maybe.withDefault "ERROR"
              model.roa.selectedOption

        weight =
          case model.weightChoose of
            Quantitative ->
              Quan
                (Log.wu_from <|
                  Maybe.withDefault "ERROR"
                    model.weightQuan.unit.selectedOption)
                model.weightQuan.amount
            Qualitative ->
              Qual
                (Log.dq_from <|
                  Maybe.withDefault "ERROR"
                    model.weightQual.selectedOption)
      in
      ( model,
        Effect.fromCmd <| Storage.logData
          log_time
          (data_time, DrugAdmin drug roa weight)
          storage
      )




-- VIEW

viewDrug model =
  case model.drug of
    Nothing ->
      h 3 model.searchError
    Just data ->
      [ h 2 data.name |> el [Font.extraBold]
      , Element.image [fill |> maximum 210 |> width, shrink |> (maximum 210) |> height]
        { src = Maybe.withDefault "/images/PSwiki_eye.svg" data.image
        , description = data.name
        }
      , Element.newTabLink [Font.color blue]
        { url = data.url
        , label = p ("Read more about " ++ data.name)
        }
      ]
      |> column [Background.color white, padding (s 3), spacing (s 3)]

viewSearch value =
  Input.text []
    { label = Input.labelAbove [] (text "search for your drug on PsychonautWiki:" |> el [Font.size (s 1)])
    , onChange = ChangedInput
    , placeholder = Just (Input.placeholder [] (text "drug name"))
    , text = value
    }

viewDetailsHeader txt =
  h 2 txt |> el [Font.color orangeLight]

viewROA model =
  let
    morespace =
      if model.roa.selectedOption == Nothing then
        [text "", text "", text "", text "", text "", text "", text "", text ""]
      else
        []
  in
  case model.drug of
    Nothing -> []
    Just d ->
      [ text ""
      , viewDetailsHeader "Select your route of administration"
      , Dropdown.view dd_roa model.roa model.roa.state
      , text ""
      , text ""
      ]
      ++ morespace
      ++ viewWeight model d.name

weightButton qua choose =
  let
    label =
      case qua of
        Quantitative -> "Quantitative"
        Qualitative -> "Qualitative"

    attrs =
      if qua == choose then
        [Font.color blue]
      else
        []
  in
  el (Events.onClick (WeightChoose qua) :: attrs) (text label)

viewWeight model drugName =
  case model.roa.selectedOption of
    Nothing ->
      []
    Just roa ->
      let
        textvalue =
          case String.fromInt model.weightQuan.amount of
            "0" -> ""
            v -> v

        more =
          case model.weightChoose of
            Quantitative ->
              row [width fill, spacing (s 2)]
                [ el [width (fillPortion 3)] <|
                  Input.text [Font.alignRight, htmlAttribute (type_ "number")]
                    { label = Input.labelHidden "enter dosage"
                    , onChange = WeightQuanChanged
                    , placeholder = Just (Input.placeholder [] (text "enter dosage"))
                    , text = textvalue
                    }
                , el [width (fillPortion 1)] <|
                  Dropdown.view dd_wu model.weightQuan.unit model.weightQuan.unit.state
                ]
                |> List.singleton
            Qualitative ->
              [ Dropdown.view dd_dq model.weightQual model.weightQual.state ]
      in
      viewDetailsHeader ("Enter dosage for " ++ roa ++ " " ++ drugName)
      :: row [width fill, spacing (s 2)]
        [ weightButton Qualitative model.weightChoose |> el [width (fillPortion 1)]
        , weightButton Quantitative model.weightChoose |> el [width (fillPortion 1)]
        ]
      :: more

viewEnd model =
  case model.roa.selectedOption of
    Nothing ->
      []
    Just _ ->
      [ minutesAgo model.minutesAgo MinutesAgoChanged
      , text ""
      , appButton Save "LOG" |> el [centerX]
      , text "", text "", text ""]

minutesAgo value msg =
  Input.text [Font.alignRight, htmlAttribute (type_ "number")]
    { label = Input.labelRight [paddingEach (bltr 0 (s 3) 0 0)] (text "minutes ago" |> el [Font.size (s 1)])
    , onChange = msg
    , placeholder = Nothing
    , text = String.fromInt value
    }

view : Model -> View Msg
view model =
  { title = "log: drug | lab rat"
  , body =
    (h 1 "Drug administration")
    :: viewSearch model.input
    :: viewDrug model
    :: text ""
    :: viewROA model
    ++ [ text ""
      , text ""
      ]
    ++ viewEnd model
  }




-- GRAPHQL

type alias Response = Maybe (List (Maybe MaybeDrug))

type alias GraphQLModel = RemoteData (Graphql.Http.Error Response) Response

query input =
  Query.substances
    (\args -> { args | query = Present input } )
    drugQuery


type alias MaybeDrug =
  { name : Maybe String
  , url : Maybe String
  , images : Maybe (List (Maybe (Maybe String)))
  }


drugQuery =
  SelectionSet.map3 MaybeDrug
    Substance.name
    Substance.url
    (Substance.images SubstanceImage.image)


makeRequest : String -> Cmd Msg
makeRequest firstInput =
  let
    secondInput = String.trim firstInput
    thirdInput = String.filter (\c -> c == ' ' |> not) secondInput
    fourthInput = String.filter (\c -> c == '-' |> not) thirdInput
    input = String.trim fourthInput
  in
  if input == "" then
    Cmd.none
  else
    query input
      |> Graphql.Http.queryRequest "https://api.psychonautwiki.org"
      |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


-- Clean data

drugData : GraphQLModel -> Maybe Drug
drugData gmodel =
  let
    drugUrl name maybeUrl =
      Maybe.withDefault ("https://psychonautwiki.org/wiki/" ++ name) maybeUrl
  in
  case gmodel of
    Loading -> Nothing
    NotAsked -> Nothing
    Failure _ -> Nothing
    Success response ->
      case response of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (d::_) ->
          case d of
            Nothing -> Nothing
            Just drug ->
              case drug.name of
                Nothing -> Nothing
                Just name ->
                  Just
                    { name = name
                    , url = drugUrl name drug.url
                    , image = drugImage drug.images
                    }

drugImage maybeImages =
  let
    removeMaybes mm =
      case mm of
        Nothing -> Nothing
        Just s -> s

    forbiddenWords str =
      String.contains "Child" str
      || String.contains "Volume" str
      || String.contains "Eye" str
      || String.contains "User" str
      || String.contains "Gears" str
      || String.contains "Infinity" str
      || String.contains "Star" str
      || String.contains "Lock" str
      || String.contains "Chain" str
      || String.contains "After" str

    imageList = case maybeImages of
      Nothing -> []
      Just list -> List.filterMap removeMaybes list

    svgList = List.filter (String.endsWith ".svg") imageList

    images = List.filter (forbiddenWords >> not) svgList
  in
  case images of
    [] -> Nothing
    i::_ -> Just i