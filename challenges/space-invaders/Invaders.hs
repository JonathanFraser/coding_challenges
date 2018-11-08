module Invaders where 

import Graphics.Gloss
import Data.Fixed

data Pixel = F | B

type Sprite = [[Pixel]]

drawPixel :: (Float,Float) -> Pixel -> Picture
drawPixel (w,h) F = rectangleSolid w h 
drawPixel _ B = blank

xStagger :: Float -> Int -> Picture -> Picture
xStagger w i = translate (w*fromIntegral i) 0.0

yStagger :: Float -> Int -> Picture -> Picture
yStagger h i = translate 0.0 ((-h)*fromIntegral i)

drawList :: (Int -> Picture -> Picture) -> (a -> Picture) -> [a] -> Picture
drawList stagger plot lst = pictures $ fmap (\(i,p) -> stagger i $ plot p) $ zip [0..] lst

draw2D :: (Float,Float) -> (a->Picture) -> [[a]] -> Picture 
draw2D (w,h) draw pixels = translate (w/2) (-h/2) $ drawList (yStagger h) (drawList (xStagger h) draw) pixels

drawSprite :: (Float,Float) -> [[Pixel]] -> Picture 
drawSprite (w,h) pixels = draw2D (w,h) (drawPixel (w,h)) pixels

animateSprites :: [Sprite] -> (Float,Float) -> Float -> Float -> Picture 
animateSprites images size delta time = drawSprite size $ images !! idx 
            where
                iters = round (time/delta)
                idx = mod iters (length images)

basic1 = [ 
    [B,B,F,B,B,B,B,B,F,B,B],
    [B,B,B,F,B,B,B,F,B,B,B],
    [B,B,F,F,F,F,F,F,F,B,B],
    [B,F,F,F,F,F,F,F,F,F,B],
    [F,F,F,F,F,F,F,F,F,F,F],
    [F,B,F,F,F,F,F,F,F,B,F],
    [F,B,F,B,B,B,B,B,F,B,F],
    [B,B,B,F,F,B,F,F,B,B,B]
    ]

basic2 = [ 
    [B,B,F,B,B,B,B,B,F,B,B],
    [F,B,B,F,B,B,B,F,B,B,F],
    [F,B,F,F,F,F,F,F,F,B,F],
    [F,F,F,F,F,F,F,F,F,F,F],
    [F,F,F,F,F,F,F,F,F,F,F],
    [B,B,F,F,F,F,F,F,F,B,B],
    [B,B,F,B,B,B,B,B,F,B,B],
    [B,F,B,B,B,B,B,B,B,F,B]
    ]

basic = animateSprites [basic1,basic2]

spear1 = [
    [B,B,B,F,F,B,B,B],
    [B,B,F,F,F,F,B,B],
    [B,F,B,F,F,B,F,B],
    [F,F,B,F,F,B,F,F],
    [F,F,F,F,F,F,F,F],
    [B,B,F,F,F,F,B,B],
    [B,F,F,F,F,F,F,B],
    [F,B,F,B,B,F,B,F]
    ]

spear2 = [
    [B,B,B,F,F,B,B,B],
    [B,B,F,F,F,F,B,B],
    [B,F,B,F,F,B,F,B],
    [F,F,B,F,F,B,F,F],
    [F,F,F,F,F,F,F,F],
    [B,B,F,B,B,F,B,B],
    [B,F,B,B,B,B,F,B],
    [B,B,F,B,B,F,B,B]
    ]

spear = animateSprites [spear1,spear2]