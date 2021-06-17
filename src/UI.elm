module UI exposing (appLayout, blrt, p, appButton, opacityFromBool)

import UIColor exposing (..)
import Element exposing (Element, alpha, centerX, column, el, explain, fill, fillPortion, height, layout, link, minimum, padding, paragraph, pointer, row, shrink, spaceEvenly, spacing, text, transparent, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



font = Font.family [Font.typeface "Cutive Mono", Font.monospace]

-- APP

appLayout: Element msg -> List (Html msg)
appLayout pageContent =
    [header] ++ [pageContent] ++  footer
    |> column [width fill, height fill]
    |> layout [font] |> List.singleton


-- SHARED

header = row [padding 20, width fill, spaceEvenly, Border.widthEach (blrt 1 0 0 0)]
    [ link [] {url = "/", label = text "lab rat"} |> el [width (fillPortion 1), Font.alignLeft]
    , text "LOG" |> el [width (fillPortion 1), Font.center]
    , text "PLAY" |> el [width (fillPortion 1), Font.alignRight]
    ]

footer = []

-- HELPERS

s = Element.modular 16 1.25 >> round

blrt : Int -> Int -> Int -> Int -> { bottom : Int, left : Int, right : Int, top : Int}
blrt b l r t =
    { bottom = b
    , left = l
    , right = r
    , top = t
    }

p : String -> Element msg
p str = paragraph [] [text str]

appButton msg label =
    Input.button
        [ padding (s 3)
        , pointer
        , width (minimum (s 10) shrink)
        , Font.color white
        , Background.color orangeLight
        , Border.rounded (s -1)
        , Element.focused
            [ Background.color red ]
        ]
        { onPress = Just msg
        , label = text label |> el [centerX]
        }

opacityFromBool from content =
    if from then
        el [transparent False, alpha 0.4321] content
    else
        el [] content
