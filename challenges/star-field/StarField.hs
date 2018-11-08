import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import System.Random 

width = 400
height = 400 

rad = sqrt (width**2 + height**2)

data Star = Star Float Float Float

apparentX :: Star -> Float 
apparentX (Star x y z) = (x/z)

apparentY :: Star -> Float 
apparentY (Star x y z) = (y/z)

apparentS :: Star -> Float 
apparentS (Star x y z) = 200 / sqrt (x**2 + y**2 + (rad*z)**2)

norm :: (RandomGen g,Fractional f) => g -> (f,g)
norm g = (f,g2) 
            where 
                (s,e) = genRange g
                (i,g2) = next g 
                f = (fromIntegral (i-s))/(fromIntegral (e-s))

instance Random Star where 
    random g = (Star x y z, g4) where 
            (ix,g2) = norm g
            (iy,g3) = norm g2
            (iz,g4) = norm g3
            x = width*ix - width/2
            y = height*iy - height/2
            z = iz

step :: (RandomGen g, Random a) => ([a],g) -> ([a],g)
step (l,g) = (v:l,g2) where (v,g2) = random g 

generateN :: RandomGen g => Int -> g -> ([Star],g)
generateN n g = foldl (\x v -> step x) ([],g) [1..n] 

drawStar :: Star -> Picture
drawStar s = translate (apparentX s) (apparentY s) $ color white $ circleSolid (apparentS s)

drawField :: [Star] -> Picture
drawField stars = pictures $ map drawStar stars

updateField :: Float -> Float -> [Star] -> IO [Star]
updateField t v stars = do 
                let forward = fmap (\(Star x y z) -> Star x y (z-v*t)) stars
                let pruned = filter (\s -> abs(apparentX s) < width/2 && (apparentY s) < height/2) forward
                let delta = length stars - length pruned
                extras <- getStdRandom (generateN delta)
                let append = fmap (\(Star x y z) -> Star x y 1) extras
                let total = pruned++append
                return total
                    

drawIO :: [Star] -> IO Picture
drawIO x = return $ drawField x

updateIO :: ViewPort -> Float -> [Star] -> IO [Star]
updateIO v t s = updateField t 1 s

disp :: Display
disp = InWindow "Star Field" (400,400) (10,10)

main :: IO ()
main = do 
    intial <-  getStdRandom (generateN 100)
    simulateIO disp black 100 intial drawIO updateIO
