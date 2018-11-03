import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Paths_challenge

width = 400
height = 400 


disp :: Display
disp = InWindow "Space Invaders" (400,400) (10,10)

main :: IO ()
main = do
    path <- getDataFileName "images/spaceinvaders.png"
    pic <- loadJuicyPNG path
    case pic of
        Just a -> display disp green a
        Nothing -> print "could not load pic"