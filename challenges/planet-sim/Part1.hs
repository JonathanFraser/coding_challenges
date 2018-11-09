import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate


width = 400
height = 400 

disp :: Display
disp = InWindow "Planet Sim Part 1" (400,400) (10,10)

main :: IO ()
main = do 
    display disp black blank
