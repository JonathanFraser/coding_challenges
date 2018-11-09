import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Vector
import System.Random 
import Control.Monad.Random
import qualified QuadTree as QT

data P = P Int Point deriving (Show)

instance Eq P where 
    (==) (P i _) (P j _) = i == j 

instance Ord P where 
    (<) (P i _) (P j _) = i < j

instance QT.At P where 
    at (P _ p) = p 

setId:: P -> Int -> P 
setId (P _ s) i = P i s

ident :: P -> Int 
ident (P i s) = i 

instance Random P where 
    randomR (P i (x1,y1),P j (x2,y2)) g = (P k (x,y),g3) 
                        where 
                            (x,g1) = randomR (x1,x2) g
                            (y,g2) = randomR (y1,y2) g1
                            (k, g3) = randomR (i,j) g2 
                            

width = 400
height = 400 

scatterInit :: Int -> QT.Rect -> IO [P]
scatterInit n (tl,br) = do 
            ps <- sequence $ take n $ repeat $ getStdRandom $ randomR (P 0 tl,P 0 br)
            let res = zipWith setId ps [0..]
            return res 

drawRect :: QT.Rect -> Picture 
drawRect ((x1,y1),(x2,y2)) = translate ((x1+x2)/2) ((y1+y2)/2) $ rectangleWire (x2-x1) (y2-y1)

radius = 5

dist :: P -> P -> Float
dist (P _ (x1,y1)) (P _ (x2,y2)) = sqrt $ (x1-x2)**2 + (y1-y2)**2

drawP :: P -> Picture 
drawP (P _ (x,y)) = translate x y $ circleSolid radius

collide :: QT.QuadTree P -> P -> Bool 
collide tree (P i (x,y)) = length lens /= 0
                        where 
                         candidates = QT.get tree ((x-radius-1,y+radius+1),(x+radius+1,y-radius-1)) :: [P]
                         ds = fmap (dist (P i (x,y))) $ filter (\p -> p /= (P i (x,y)))candidates 
                         lens = filter (\d -> d < 2*radius) ds

draw:: QT.Rect -> [P] -> IO Picture
draw treebox pts = do 
                    let tree = QT.fromList treebox pts :: QT.QuadTree P
                    let colorize b = if b then color red else color white :: Picture -> Picture
                    let colliding = filter (collide tree) pts 
                    let notcolliding = filter (\x -> not $ collide tree x) pts
                    let coldraw = fmap (\ x -> color red $ drawP x) colliding
                    let missdraw = fmap (\ x -> color white $ drawP x) notcolliding
                    return $ pictures (coldraw ++ missdraw)



add :: P -> P -> P 
add (P i (x1,y1)) (P _ (x2,y2)) = P i (x1+x2,y1+y2)

jitter = ((-1,1),(1,-1))

update :: [P] -> IO [P]
update ps = do 
            fs <- scatterInit (length ps) jitter
            return $ zipWith add ps fs

disp :: Display
disp = InWindow "Quad Tree 2" (400,400) (10,10)

total = ((-200,200),(200,-200))

main :: IO ()
main = do 
    pts <- scatterInit 1000 total
    simulateIO disp black 30 pts (draw total) (\_ _ i -> update i)