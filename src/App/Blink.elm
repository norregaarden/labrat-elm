port module App.Blink exposing (blink, JSimage, imageProperties, Image, allImages, placeholderImage, imageString)

-- 1,2,4,8,16,32,64,128,256

import Json.Encode as E

port blink : E.Value -> Cmd msg

type alias JSimage =
  { src : String
  , description : String
  }

imageProperties : Image -> JSimage
imageProperties billede =
  { src = "/images/husk/" ++ imageString billede ++ ".svg"
  , description = imageString billede
  }

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
