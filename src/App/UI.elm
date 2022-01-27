module UI exposing
  ( appLayout
  , spilTitel, s, sf, bltr
  , p, small, h
  , appButton, smallAppButton
  , smallAppButtonDisabled
  , flatFillButton
  , showWhen, showListWhen
  )

import Element.Region as Region
import UIColor exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Gen.Route as Route exposing (Route)

---- SETTINGS ----

font =
  Font.family [Font.typeface "Source Serif 4", Font.sansSerif]

buttonShadow =
 Border.shadow
   { offset = ( 0.5, 0.5 )
   , size = 0.0001
   , blur = 3
   , color = black
   }

buttonFontShadow =
  Font.shadow { offset = ( 0.1, 0.1 ), blur = 0.1, color = black}


---- META ----

spilTitel titel = "play: " ++ titel ++ " | lab rat"


---- APP ----

appLayout: Route -> List (Element msg) -> List (Html msg)
appLayout headerRoute pageContent =
  [header headerRoute] ++ [column [paddingXY (s 2) (s 6), spacing (s 2), width fill] pageContent] ++  footer
  |> column [width fill, height fill]
  |> layout [font, Font.light, Background.color white] |> List.singleton


---- SHARED VIEW ----

headerLink string route r =
  let
    bg =
      if route == r then
        [Font.color orangeLight]
      else
        []

    headerLinkAttributes =
      bg ++ [width (fillPortion 1), Font.center, Font.size (s 3), height fill]

    routeText txt
      = (text txt |> el [centerY, centerX])
  in
  routeLink (routeText string) route (headerLinkAttributes)

header r = row
  [ paddingXY (s -2) 0
  , width fill
  --, Border.widthEach (bltr 1 0 0 0)
  , Background.color (rgba 1 1 1 0.666)
  ]
    [ routeLink labratlogo Route.Home_ []
      |> el [width shrink, paddingXY 0 (s -3), width (maximum (42*2) (fillPortion 1)) ]
    , row [paddingXY (s -1) 0, width fill, height fill, spacing -1, width (fillPortion 6) ]
      [ headerLink "DATA" Route.Data r
      , headerLink "LOG" Route.LogChoose r
      , headerLink "PLAY" Route.Play r
      ]
    ]

labratlogo =
  column
    [ Font.size (s 1)
    , Border.rounded (s -1)
    , width fill
    ]
    [ image [width (px 50), height (px 50)]
      { src = "/images/rotteHjerneLogo.svg"
      , description = "lab rat logo"
      }
    ]

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

small : String -> Element msg
small str = paragraph [Font.size (s 1)] [text str]

h : Int -> String -> Element msg
h n str =
  if n == 1 then
    paragraph
      [Region.heading n, Font.size (s (5 - n)), Font.color blue, Font.extraBold
      , paddingXY 0 (s 0)
      ]
      [text str]
  else
    paragraph
      [Region.heading n, Font.size (s (5 - n))]
      [text str]

appButton msg label =
  Input.button
    [ paddingXY (s 5) (s 3)
    , width (minimum (s 11) shrink)
    , Font.color black
    --, Font.extraBold
    , Font.size (s 4)
    , Background.color orangeLight
    , Border.rounded (s -5)
    --, buttonShadow
    --, buttonFontShadow
    , Element.focused
      [ Background.color blue ]
    ]
    { onPress = Just msg
    , label = text label |> el [centerX]
    }

smallAppButton msg label =
  Input.button
    [ paddingXY (s 3) (s 2)
    , width (minimum (s 9) shrink)
    , Font.color black
    --, Font.extraBold
    , Font.size (s 2)
    , Background.color orangeLight
    , Border.rounded (s -5)
    , Element.focused
        [ Background.color blue ]
    --, buttonShadow
    --, buttonFontShadow
    ]
    { onPress = Just msg
    , label = text label |> el [centerX]
    }

flatFillButton msg label =
  Input.button
    [ paddingXY (s 3) (s 2)
    , width fill
    , Font.color black
    --, Font.extraBold
    , Font.size (s 2)
    , Background.color orangeLight
    , Border.rounded (s -5)
    --, Element.focused [ Background.color blue ]
    ]
    { onPress = Just msg
    , label = text label |> el [centerX]
    }


smallAppButtonDisabled label =
  Input.button
    [ padding (s 1)
    , width (minimum (s 9) shrink)
    , Font.color white
    , Font.extraBold
    , Font.size (s 2)
    , Background.color red
    , Border.rounded 0
    , Element.focused
        [ Background.color red ]
    ]
    { onPress = Nothing
    , label = text label |> el [centerX]
    }

routeLink label route attributes =
  link attributes { url = Route.toHref route, label = label }

-- conditional display
showWhen : Bool -> Element msg -> Element msg
showWhen should show =
  if should then show
  else none

showListWhen : Bool -> List (Element msg) -> List (Element msg)
showListWhen should show =
  if should then show
  else []