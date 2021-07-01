module Husk exposing (Image, allImages, placeholderImage, imageString)


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