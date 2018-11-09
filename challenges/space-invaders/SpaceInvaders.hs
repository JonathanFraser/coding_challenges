import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Bitmap
import Invaders 
import Invaders

width = 400
height = 400 


disp :: Display
disp = InWindow "Space Invaders" (400,400) (10,10)


type Animation =  Float -> Picture

animations :: [Animation] -> Float -> Picture 
animations an f = pictures $ fmap (\x -> x f) an


space = [
    \x -> translate (-30) (-100) $ color (dark green) $ block (10,10) 0.2 x,
    \x -> translate 30 100 $ color (dark blue) $ spear (10,10) 0.3 x,
    \x -> translate 30 0 $ color (light red) $ basic (10,10) 0.25 x
    ]
main :: IO ()
main = animate disp black $ animations space