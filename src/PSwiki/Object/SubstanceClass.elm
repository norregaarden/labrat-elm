-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PSwiki.Object.SubstanceClass exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import PSwiki.InputObject
import PSwiki.Interface
import PSwiki.Object
import PSwiki.Scalar
import PSwiki.ScalarCodecs
import PSwiki.Union


chemical : SelectionSet (Maybe (List (Maybe String))) PSwiki.Object.SubstanceClass
chemical =
    Object.selectionForField "(Maybe (List (Maybe String)))" "chemical" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


psychoactive : SelectionSet (Maybe (List (Maybe String))) PSwiki.Object.SubstanceClass
psychoactive =
    Object.selectionForField "(Maybe (List (Maybe String)))" "psychoactive" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)
