import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Vector
import System.Random 
import Control.Monad.Random
import Linear.V2
import Linear.Vector
import Linear.Metric

width = 400
height = 400 

expandPath :: V2 Float -> V2 Float -> [V2 Float]
expandPath start end = [start,fq,mid,lq]
    where 
        fq = lerp 0.33 end start 
        mid_c = lerp 0.5 end start 
        lq = lerp 0.66 end start 
        pervec = perp (end - start) :: V2 Float 
        n = normalize pervec :: V2 Float 
        mid = mid_c+(n ^* (distance fq start))


getPath :: Int -> [V2 Float]
getPath 0 = [V2 (-1) 1, V2 1 1, V2 0 (-1)]
getPath n = mids++end where 
                prev = getPath (n-1)
                mids = concat $ zipWith expandPath prev (tail prev) 
                end = expandPath (last prev) (head prev)

drawSnowFlake :: Int -> Picture
drawSnowFlake level = lineLoop $ fmap (\(V2 a b) -> (a,b)) $ getPath level 

disp :: Display
disp = InWindow "Koch Snowflake" (400,400) (10,10)

main :: IO ()
main = do 
    display disp black (scale 100 100 $ color white $ drawSnowFlake 6)