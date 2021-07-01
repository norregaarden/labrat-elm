module Husk exposing (Image, allImages, placeholderImage, fixImage, imageString)


type Image
  = LibertyCap
  | Coca
  | GoldenTeacher
  | HawaiianWoodrose
  | Peyote
  | Poppy
  | SanPedro
  | SonoranDesertToad
  | Tobacco


allImages : List Image
allImages =
  [ LibertyCap
  , Coca
  , GoldenTeacher
  , HawaiianWoodrose
  , Peyote
  , Poppy
  , SanPedro
  , SonoranDesertToad
  , Tobacco
  ]



-- HELPERS

placeholderImage =
  LibertyCap


fixImage : ( Maybe Image, List Image ) -> Image
fixImage ( maybe, _ ) =
  case maybe of
    Nothing -> placeholderImage
    Just image -> image


imageString : Image -> String
imageString x =
  case x of
    LibertyCap ->
      "LibertyCap"
    Coca ->
      "Coca"
    GoldenTeacher ->
      "GoldenTeacher"
    HawaiianWoodrose ->
      "HawaiianWoodrose"
    Peyote ->
      "Peyote"
    Poppy ->
      "Poppy"
    SanPedro ->
      "SanPedro"
    SonoranDesertToad ->
      "SonoranDesertToad"
    Tobacco ->
      "Tobacco"