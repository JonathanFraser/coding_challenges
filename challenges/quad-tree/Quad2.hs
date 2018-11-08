import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Vector
import System.Random 
import Control.Monad.Random
import qualified QuadTree as QT

data P = P Point deriving (Show,Ord,Eq)

instance QT.At P where 
    at (P p) = p 

instance Random P where 
    randomR (P (x1,y1),P (x2,y2)) g = (P (x,y),g2) 
                        where 
                            (x,g1) = randomR (x1,x2) g
                            (y,g2) = randomR (y1,y2) g1  

width = 400
height = 400 

scatterInit :: Int -> QT.Rect -> IO [P]
scatterInit n (tl,br) = sequence $ take n $ repeat $ getStdRandom $ randomR (P tl,P br)

drawRect :: QT.Rect -> Picture 
drawRect ((x1,y1),(x2,y2)) = translate ((x1+x2)/2) ((y1+y2)/2) $ rectangleWire (x2-x1) (y2-y1)

drawP :: P -> Picture 
drawP (P (x,y)) = translate x y $ circleSolid 1

draw:: QT.Rect -> QT.Rect -> [P] -> Picture
draw sel treebox pts = pictures [color white $ simple, color green $ sampled, color red $ drawRect sel]
                    where 
                        tree = QT.fromList treebox pts 
                        simple = pictures $ concatMap (\(b,p) -> [drawRect b, drawP p]) $ QT.toList tree
                        sampled = pictures $ fmap drawP $ QT.get tree sel 

disp :: Display
disp = InWindow "Quad Tree 2" (400,400) (10,10)

total = ((-200,200),(200,-200))
sel = ((-50,150),(150,-50))

main :: IO ()
main = do 
    pts <- scatterInit 5000 total
    display disp black (draw sel total pts)