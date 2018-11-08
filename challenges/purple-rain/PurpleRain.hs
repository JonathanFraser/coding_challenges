import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import System.Random 

width = 400
height = 400 

data Rain = Rain Float Float Float 

norm :: (RandomGen g,Fractional f) => g -> (f,g)
norm g = (f,g2) 
            where 
                (s,e) = genRange g
                (i,g2) = next g 
                f = (fromIntegral (i-s))/(fromIntegral (e-s))

instance Random Rain where 
    random g = (Rain x y z, g4) where 
            (ix,g2) = norm g
            (iy,g3) = norm g2
            (iz,g4) = norm g3
            x = width*ix - width/2
            y = height*iy - height/2
            z = iz

step :: (RandomGen g, Random a) => ([a],g) -> ([a],g)
step (l,g) = (v:l,g2) where (v,g2) = random g 

generateN :: RandomGen g => Int -> g -> ([Rain],g)
generateN n g = foldl (\x v -> step x) ([],g) [1..n] 

drawRain :: Rain -> Picture
drawRain (Rain x y v) = color violet $ translate x y $ rectangleSolid (v*2) (20*v)

drawField :: [Rain] -> Picture
drawField drops = pictures $ map drawRain drops

updateField :: Float -> Float -> [Rain] -> IO [Rain]
updateField t v stars = do 
                let forward = fmap (\(Rain x y v) -> Rain x (y - (v+0.1)*t*300) v) stars
                let pruned = filter (\(Rain x y v) -> y > -210) forward
                let delta = length stars - length pruned
                extras <- getStdRandom (generateN delta)
                let append = fmap (\(Rain x y v) -> Rain x 210 v) extras
                let total = pruned++append
                return total
                    

drawIO :: [Rain] -> IO Picture
drawIO x = return $ drawField x

updateIO :: ViewPort -> Float -> [Rain] -> IO [Rain]
updateIO v t s = updateField t 1 s

disp :: Display
disp = InWindow "Star Field" (400,400) (10,10)

main :: IO ()
main = do 
    intial <-  getStdRandom (generateN 1000)
    simulateIO disp (light violet) 100 intial drawIO updateIO
