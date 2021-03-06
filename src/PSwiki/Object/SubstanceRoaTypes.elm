-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module PSwiki.Object.SubstanceRoaTypes exposing (..)

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


oral :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
oral object____ =
    Object.selectionForCompositeField "oral" [] object____ (identity >> Decode.nullable)


sublingual :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
sublingual object____ =
    Object.selectionForCompositeField "sublingual" [] object____ (identity >> Decode.nullable)


buccal :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
buccal object____ =
    Object.selectionForCompositeField "buccal" [] object____ (identity >> Decode.nullable)


insufflated :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
insufflated object____ =
    Object.selectionForCompositeField "insufflated" [] object____ (identity >> Decode.nullable)


rectal :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
rectal object____ =
    Object.selectionForCompositeField "rectal" [] object____ (identity >> Decode.nullable)


transdermal :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
transdermal object____ =
    Object.selectionForCompositeField "transdermal" [] object____ (identity >> Decode.nullable)


subcutaneous :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
subcutaneous object____ =
    Object.selectionForCompositeField "subcutaneous" [] object____ (identity >> Decode.nullable)


intramuscular :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
intramuscular object____ =
    Object.selectionForCompositeField "intramuscular" [] object____ (identity >> Decode.nullable)


intravenous :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
intravenous object____ =
    Object.selectionForCompositeField "intravenous" [] object____ (identity >> Decode.nullable)


smoked :
    SelectionSet decodesTo PSwiki.Object.SubstanceRoa
    -> SelectionSet (Maybe decodesTo) PSwiki.Object.SubstanceRoaTypes
smoked object____ =
    Object.selectionForCompositeField "smoked" [] object____ (identity >> Decode.nullable)
