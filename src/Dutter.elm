module Dutter exposing (Dut, Farve, Form, alleDutter, enDut, svgDut)

-- 4xFarver x 4xFormer = 16
--import Color exposing (Color)

import Color exposing (..)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)


enDut =
    ( Grøn, Cirkel )


dutBredde =
    100


dutHøjde =
    dutBredde


dutCentrum =
    50


dutBreddeStr =
    fromInt dutBredde


dutHøjdeStr =
    fromInt dutHøjde


dutCentrumStr =
    fromInt dutCentrum


dutViewbox =
    "0 0 " ++ dutBreddeStr ++ " " ++ dutHøjdeStr


type alias Dut =
    ( Farve, Form )


type Form
    = Trekant
    | Firkant
    | Sekskant
    | Cirkel


alleFormer : List Form
alleFormer =
    [ Trekant, Firkant, Sekskant, Cirkel ]


type Farve
    = Rød
    | Gul
    | Grøn
    | Blå


alleFarver : List Farve
alleFarver =
    [ Rød, Gul, Grøn, Blå ]


alleDutter : List Dut
alleDutter =
    List.concatMap (\x -> List.map (\y -> ( x, y )) alleFormer) alleFarver


svgDut størrelse dut =
    let
        str =
            String.fromFloat størrelse
    in
    svg [ width str, height str, viewBox dutViewbox ]
        [ tegnDut dut ]


tegnDut : Dut -> Svg msg
tegnDut ( farve, form ) =
    let
        rgb =
            toCssString (tegnFarve farve)

        --col = "rgba(" ++ fromFloat rgb.red ++ "," ++ fromFloat rgb.green ++ "," ++ fromFloat rgb.blue ++ "," ++ fromFloat rgb.alpha ++ ")"
    in
    case form of
        Trekant ->
            polygon [ points "50,0 0,100 100,100", fill rgb ] []

        Firkant ->
            rect [ x "0", y "0", width "100", height "100", fill rgb ] []

        Sekskant ->
            polygon [ points "29,0 71,0 100,50 71,100 29,100 0,50", fill rgb ] []

        Cirkel ->
            circle [ cx dutCentrumStr, cy dutCentrumStr, r dutCentrumStr, fill rgb ] []


tegnFarve : Farve -> Color
tegnFarve f =
    case f of
        Rød ->
            darkRed

        Gul ->
            darkYellow

        Grøn ->
            darkGreen

        Blå ->
            darkBlue
