module Helpers.DropdownConfig exposing (..)

import Dropdown
import Element exposing (el, fill, padding, paddingXY, pointer, px, rgb255, rotate, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import UI exposing (s)
import UIColor exposing (white)


dropdownConfig placeholder options dropdownMsg optionPickedMsg =
    let
        containerAttrs =
            [ Background.color white
            , Border.rounded 5
            , Font.size (s 3)
            , pointer, width fill ]

        selectAttrs =
            [ Border.width 1
            , Border.color gray
            , Border.rounded 5
            , paddingXY 16 8
            , spacing 10
            , width fill ]

        searchAttrs =
            [ Border.width 0, padding 0 ]

        listAttrs =
            [ Border.width 1
            , Border.color gray
            , Background.color white
            , Border.rounded 5
            , width fill
            , spacing 5
            ]

        itemToPrompt item =
            text item

        gray = rgb255 100 100 100

        itemToElement selected highlighted i =
            let
                bgColor =
                    if selected then
                        gray

                    else
                        rgb255 255 255 255
            in
            row
                [ Background.color bgColor
                , Border.rounded 5
                , padding 8
                , spacing 10
                , width fill
                ]
                [ el [] (text "-")
                , el [ Font.size 16 ] (text i)
                ]
    in
    Dropdown.filterable
        { itemsFromModel = always options
        , selectionFromModel = .selectedOption
        , dropdownMsg = dropdownMsg
        , onSelectMsg = optionPickedMsg
        , itemToPrompt = itemToPrompt
        , itemToElement = itemToElement
        , itemToText = identity
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withOpenCloseButtons { openButton = text "^" |> el [rotate 3.14], closeButton = text "^" }
        |> Dropdown.withPromptElement (el [Font.color gray] (text placeholder))
        |> Dropdown.withFilterPlaceholder placeholder
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs