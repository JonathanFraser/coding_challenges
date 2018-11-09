import Graphics.Gloss 


width = 400
height = 400 

type Tree = [(Point)]


drawSimpleBranch:: Picture
drawSimpleBranch = line [(0,0),(0,1)]

drawThickBranch:: Picture
drawThickBranch = translate 0 0.5 $ rectangleSolid 0.1 1

drawBud = circleSolid 0.2

xfrm :: Point -> Picture -> Picture
xfrm (x,y) p = translate 0 1 $ scale s s $ rotate (180/pi*atan2 x y) p 
                                            where s = sqrt (x**2 + y**2)

drawTree :: Int -> Tree -> (Int -> Picture) -> Picture -> Picture 
drawTree 0 t p bud = bud 
drawTree n t p bud = pictures $ [p n] ++ fmap (\pt-> xfrm pt (drawTree (n-1) t p bud)) t

disp :: Display
disp = InWindow "Fractal Tree" (400,400) (10,10)

main :: IO ()
main = do 
    let tree = [(-0.5,0.5),(0.5,0.5),(0,0.5)]
    display disp black (translate 0 (-200) $ scale 100 100 $ drawTree 8 tree (\x -> color (dark orange) $ drawThickBranch) (color (dark green) $ drawBud))