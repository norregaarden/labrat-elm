module UI exposing
  ( appLayout
  , spilTitel, s, sf, bltr, p
  , appButton, smallAppButton
  , smallAppButtonDisabled
  , showWhen, showListWhen
  )

import UIColor exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Gen.Route as Route exposing (Route)

---- SETTINGS ----

font = Font.family [Font.typeface "Cutive Mono", Font.monospace]


---- META ----

spilTitel titel = "Play: " ++ titel ++ " | lab rat"


---- APP ----

appLayout: List (Element msg) -> List (Html msg)
appLayout pageContent =
    [header] ++ [column [paddingXY (s 2) (s 5), spacing (s 2), width fill] pageContent] ++  footer
    |> column [width fill, height fill]
    |> layout [font] |> List.singleton


---- SHARED VIEW ----

headerLinkAttributes =
  [width (fillPortion 1), Font.center, Font.size (s 3)]

header = row [padding (s -2), width fill, Border.widthEach (bltr 1 0 0 0)]
    [ routeLink labratlogo Route.Home_ [] |> el [width shrink]
    , row [paddingXY (s -1) 0, width fill]
        [ routeLink (text "DATA") Route.Data headerLinkAttributes
        --, text "LOG" |> el [width (fillPortion 1), Font.center, Font.size (s 3)]
        --, link [width (fillPortion 1), Font.center, Font.size (s 3)] {url = "/log/string", label = text "LOG"}
        , routeLink (text "LOG") Route.LogChoose headerLinkAttributes
        , routeLink (text "PLAY") Route.Play headerLinkAttributes
        ]
    ]

labratlogo = column [Font.size (s 2), Font.color white, Background.color black] [text "lab", text "rat"]

footer = []


---- HELPERS ----

-- scale - use for all sizes
s : Int -> Int
s = sf >> round

sf : Int -> Float
sf = Element.modular 16 1.25

-- for e.g. Border.widthEach, paddingEach?
bltr : Int -> Int -> Int -> Int -> { bottom : Int, left : Int, right : Int, top : Int}
bltr b l t r =
    { bottom = b
    , left = l
    , top = t
    , right = r
    }

-- text wrapping
p : String -> Element msg
p str = paragraph [] [text str]

appButton msg label =
    Input.button
        [ padding (s 3)
        , pointer
        , width (minimum (s 11) shrink)
        , Font.color white
        , Font.extraBold
        , Font.size (s 3)
        , Background.color orangeLight
        , Border.rounded (s -1)
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just msg
        , label = text label |> el [centerX]
        }

smallAppButton msg label =
    Input.button
        [ padding (s 1)
        , pointer
        , width (minimum (s 9) shrink)
        , Font.color white
        , Font.extraBold
        , Font.size (s 2)
        , Background.color orangeLight
        , Border.rounded (s -1)
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just msg
        , label = text label |> el [centerX]
        }

smallAppButtonDisabled label =
    Input.button
        [ padding (s 1)
        , pointer
        , width (minimum (s 9) shrink)
        , Font.color white
        , Font.extraBold
        , Font.size (s 2)
        , Background.color red
        , Border.rounded (s -1)
        , Element.focused
            [ Background.color red ]
        ]
        { onPress = Nothing
        , label = text label |> el [centerX]
        }

routeLink label route attributes =
  link attributes { url = Route.toHref route, label = label }

-- not used anymore
--opacityFromBool : Bool -> Element msg -> Element msg
opacityFromBool from content =
    if from then
        el [transparent False, alpha 0.4321] content
    else
        el [] content

-- conditional display
showWhen : Bool -> Element msg -> Element msg
showWhen should show =
    if should then show
    else none

showListWhen : Bool -> List (Element msg) -> List (Element msg)
showListWhen should show =
    if should then show
    else []