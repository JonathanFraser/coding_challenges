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


main :: IO ()
main = animate disp red (\x -> color blue $ spear (10,10) 0.25 x)