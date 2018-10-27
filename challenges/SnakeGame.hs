import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Play

width = 400
height = 400 

data Snake = Snake [Point]

drawBlock :: Point -> Picture 
drawBlock pnt


disp :: Display
disp = InWindow "Snake Game" (width,height) (10,10)

main :: IO ()
main = do 
    intial <-  getStdRandom (generateN 100)
    playIO disp black 100 intial drawIO updateIO
