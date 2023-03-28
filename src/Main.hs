module Main where

import           Graphics.Gloss

type Function = Float -> Float

f :: Function
f x = 0.2 * x ^ 2 + 7 * x - 100

f' :: Function
f' = derived f

derived :: Function -> Float -> Float
derived f x = (f (x + δ) - f x) / δ
  where δ = 1.0 -- A single pixel between each point

newtons :: Function -> Function -> Float -> Int -> Float
newtons f f' x n
  | abs (f x) < ε || n == 0 = x
  | otherwise = newtons f f' (x - (f x / f' x)) (n - 1)
  where ε = 0.00001

domain :: [Float]
domain = map ((/ 10) . fromIntegral) ([-10000..10000] :: [Int])

graph :: Function -> Picture
graph f = line $ map (\x -> (x, f x)) domain

step :: Float -> Picture
step t = pictures
  [ color fnColor $ graph f
  , color tnColor $ graph $ ln (f' x_0) x_0 -- The tangent at the root
  , color fgColor $ axis
  ]
  where x_0 = newtons f f' 0 rs -- The root of f
        rs = 10 `min` floor t   -- The number of recursions

axis :: Picture
axis = pictures
  [ line [(head domain, 0), (last domain, 0)] -- The x-axis
  , line [(0, head domain), (0, last domain)] -- The y-axis
  ]

-- line from gradient and root
ln :: Float -> Float -> Float -> Float
ln a x_0 x = a * (x - x_0)

main :: IO ()
main =
  animate (InWindow "Newtons Method" (w, h) winPos) bgColor step

w, h :: Int
w = 1200
h = 1200

sW, sH :: Int
sW = 2560
sH = 1600

winPos :: (Int, Int)
winPos = (sW `div` 2 - w `div` 2,
          sH `div` 2 - h `div` 2)

fgColor, bgColor, tnColor, fnColor :: Color
fgColor = makeColorI 235 219 178 255
bgColor = makeColorI 40  40  40  255
tnColor = makeColorI 251 73  52  255
fnColor = makeColorI 184 187 38  255
