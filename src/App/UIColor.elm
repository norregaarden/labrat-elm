module UIColor exposing (..)

import Element exposing (rgb255)

-- dark theme
-- gf white = #e7eaed
-- gf black = #202225
-- https://coolors.co/001219-005f73-0a9396-94d2bd-e9d8a6-ee9b00-ca6702-bb3e03-ae2012-9b2226

black =
  rgb255 0 0 0

gray =
  rgb255 100 100 100

white =
  rgb255 255 255 255

blue =
  rgb255 0 95 115

green =
  rgb255 10 147 150

bluegreen =
  rgb255 148 210 189

beige =
  rgb255 233 216 166

orangeLight =
  rgb255 238 155 0

orangeDark =
  rgb255 202 103 2

-- redLight
red =
  rgb255 174 32 18
-- redDark



-- red green blue for play scores visuals
-- log(2(x+1/e+1/e^2+1/e^3+1/e^4+1/e^5)) + 1 - log(2(1+1/e+1/e^2+1/e^3+1/e^4+1/e^5))
-- over 2 og under 1/2 er rødt
-- 1.23 og 1/1.23 gult

scaleRatio x =
  let
    m = 0.57805537866 --1/e+1/e^2+1/e^3+1/e^4+1/e^5
    log = logBase e
  in
  log ( 2 * (x + m) ) + 1 - log ( 2 * (1 + m) )

rgb255FromRGB (RGB r g b) =
  rgb255 r g b

type RGB =
  RGB Int Int Int

scoreRed =
  RGB 210 34 45

scoreYellow =
  RGB 255 191 0

scoreGreen =
  RGB 35 136 35


wA : Int -> Int -> Float -> Int
wA low high ratio =
   ratio * (toFloat low) + (1-ratio) * (toFloat high)
   |> round

-- ratio betwen 0 and 1
weightedAverageRGB (RGB rl gl bl) (RGB rh gh bh) ratio =
  RGB (wA rl rh ratio) (wA gl gh ratio) (wA bl bh ratio)

greenToRed y =
  if y > 1 then
    weightedAverageRGB scoreYellow scoreGreen (1/y)
    |> rgb255FromRGB
  else
    weightedAverageRGB scoreYellow scoreRed y
    |> rgb255FromRGB



{-
-- y from scaleRatio
greenToRed y =
  let
    redToYellow z =


    yellowToGreen z =


  in
  if y > 1.23 then
    redToYellow y
  else
    yellowToGreen y
-}
