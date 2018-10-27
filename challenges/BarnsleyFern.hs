import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Vector
import System.Random 
import Control.Monad.Random

width = 400
height = 400 

type Map = Point -> Point
type MapSet = [(Map,Rational)]

xfrm :: (Point,Point) -> Point -> Point -> Point 
xfrm (row1,row2) (x,y) val = (x + dotV row1 val,y+dotV row2 val) 

--w	a	b	c	d	e	f	p	Portion generated
--ƒ1	0	0	0	0.16	0	0	0.01	Stem
--ƒ2	0.85	0.04	−0.04	0.85	0	1.60	0.85	Successively smaller leaflets
--ƒ3	0.20	−0.26	0.23	0.22	0	1.60	0.07	Largest left-hand leaflet
--ƒ4	−0.15	0.28	0.26	0.24	0	0.44	0.07	Largest right-hand leaflet
fernxfrm :: MapSet 
fernxfrm = [
    (xfrm ((0,0),(0,0.16)) (0,0),1/100),
    (xfrm ((0.85,0.04),(-0.04,0.85)) (0,1.6),85/100),
    (xfrm ((0.20,-0.26),(0.23,0.22)) (0,1.6),7/100),
    (xfrm ((-0.15,0.28),(0.26,0.24)) (0,0.44),7/100)
    ]


randomXfrm :: RandomGen g => MapSet -> g -> (Map,g) 
randomXfrm dat g = runRand (fromList dat) g 

ptlist :: MapSet -> Int -> Point -> IO [Point]
ptlist d n p = if n==0 then 
                    return [p]
                else do 
                    map <- getStdRandom (randomXfrm d)
                    res <- ptlist d (n-1) p 
                    return ([map $ head res]++res)

fernInit :: IO [Point]
fernInit = ptlist fernxfrm 100000 (0,0)

chooseColor :: Point -> Color 
chooseColor (x,y) = withAlpha 0.6 $ dark green 

drawPoint :: Point -> Picture 
drawPoint (x,y) = color (chooseColor (x,y)) $ translate x y $ circleSolid 0.02

drawFern :: [Point] -> Picture
drawFern pts = pictures $ fmap drawPoint pts

disp :: Display
disp = InWindow "Barnsley Fern" (400,400) (10,10)

main :: IO ()
main = do 
    fern <- fernInit
    display disp black (translate 0 (-200) $ scale 39 39 $ drawFern fern)