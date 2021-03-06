-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PSwiki.Object.SubstanceTolerance exposing (..)

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


full : SelectionSet (Maybe String) PSwiki.Object.SubstanceTolerance
full =
    Object.selectionForField "(Maybe String)" "full" [] (Decode.string |> Decode.nullable)


half : SelectionSet (Maybe String) PSwiki.Object.SubstanceTolerance
half =
    Object.selectionForField "(Maybe String)" "half" [] (Decode.string |> Decode.nullable)


zero : SelectionSet (Maybe String) PSwiki.Object.SubstanceTolerance
zero =
    Object.selectionForField "(Maybe String)" "zero" [] (Decode.string |> Decode.nullable)
