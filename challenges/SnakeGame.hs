import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game

width = 400
height = 400

data Snake = Snake [Point]

data Dir = Up | Down | Left | Right
data Game = Game Snake Dir Point

drawBlock :: Point -> Picture 
drawBlock (x,y) = translate (10*x) (10*y) $ rectangleSolid  10 10 

drawSnake :: Snake -> Picture
drawSnake (Snake pts) = color white $ pictures $ fmap drawBlock pts 

drawGame :: Game -> Picture 
drawGame (Game snk _ pt) = pictures [color red $ drawBlock pt,drawSnake snk]

updateGame :: Float -> Game -> IO Game 
updateGame _ (Game snk dir pt) = return $ Game snk dir pt

disp :: Display
disp = InWindow "Snake Game" (width,height) (10,10)

main :: IO ()
main = playIO disp black 100 intital (return.drawGame) (\_ g -> return g) updateGame
                                where 
                                    intital = Game (Snake [(3,3),(4,3),(4,4)]) Main.Up (5,5)
