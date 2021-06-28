-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PSwiki.Object.SubstanceRoaDose exposing (..)

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


units : SelectionSet (Maybe String) PSwiki.Object.SubstanceRoaDose
units =
    Object.selectionForField "(Maybe String)" "units" [] (Decode.string |> Decode.nullable)


threshold : SelectionSet (Maybe Float) PSwiki.Object.SubstanceRoaDose
threshold =
    Object.selectionForField "(Maybe Float)" "threshold" [] (Decode.float |> Decode.nullable)


heavy : SelectionSet (Maybe Float) PSwiki.Object.SubstanceRoaDose
heavy =
    Object.selectionForField "(Maybe Float)" "heavy" [] (Decode.float |> Decode.nullable)


common :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDose
common object____ =
    Object.selectionForCompositeField "common" [] object____ (identity >> Decode.nullable)


light :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDose
light object____ =
    Object.selectionForCompositeField "light" [] object____ (identity >> Decode.nullable)


strong :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDose
strong object____ =
    Object.selectionForCompositeField "strong" [] object____ (identity >> Decode.nullable)