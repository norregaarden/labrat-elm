-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PSwiki.Object.SubstanceRoaDuration exposing (..)

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


afterglow :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
afterglow object____ =
    Object.selectionForCompositeField "afterglow" [] object____ (identity >> Decode.nullable)


comeup :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
comeup object____ =
    Object.selectionForCompositeField "comeup" [] object____ (identity >> Decode.nullable)


duration :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
duration object____ =
    Object.selectionForCompositeField "duration" [] object____ (identity >> Decode.nullable)


offset :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
offset object____ =
    Object.selectionForCompositeField "offset" [] object____ (identity >> Decode.nullable)


onset :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
onset object____ =
    Object.selectionForCompositeField "onset" [] object____ (identity >> Decode.nullable)


peak :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
peak object____ =
    Object.selectionForCompositeField "peak" [] object____ (identity >> Decode.nullable)


total :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoaDurationRange
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaDuration
total object____ =
    Object.selectionForCompositeField "total" [] object____ (identity >> Decode.nullable)