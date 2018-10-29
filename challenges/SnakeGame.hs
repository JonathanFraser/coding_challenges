import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game

blockSize = 10 
blocksWide = 40
blocksHigh = 40
width = blocksWide*blockSize
height = blocksHigh*blockSize

data Block = Block (Int,Int) deriving (Eq)


data Snake = Snake [Block]
data Target = Target Block

data Dir = DirUp | DirDown | DirLeft | DirRight
data Game = Start | InPlay State | Victory | Loss 
data State = State Snake Dir Target



next :: Dir -> Snake -> Block 
next DirUp (Snake blks) = Block (a,b+1) where Block (a,b) = head blks
next DirDown (Snake blks) = Block (a,b-1) where Block (a,b) = head blks
next DirLeft (Snake blks) = Block (a-1,b) where Block (a,b) = head blks
next DirRight (Snake blks) = Block (a+1,b) where Block (a,b) = head blks

growSnake :: Snake -> Block -> Snake 
growSnake (Snake blks) next = Snake (next:blks) 

moveSnake :: Snake -> Block -> Snake 
moveSnake (Snake blks) next = Snake (next:(init blks))

collide :: Snake -> Dir -> Target -> Bool
collide snk dir (Target tgt) = tgt == next dir snk 

newBlock :: [Block] -> IO Block
newBlock snk = return $ Block (10,20)

newState :: IO State 
newState = do 
          let snk = Block (quot blocksWide 2,quot blocksHigh 2)
          tgt <- newBlock [snk]
          return $ State (Snake [snk]) DirLeft (Target tgt) 


tickState :: State -> IO State 
tickState (State snk d tgt) = if collide snk d tgt then 
                            do
                                let bsnk = growSnake snk (next d snk)
                                let (Snake blks) = bsnk
                                tgt2 <- newBlock blks
                                return $ State bsnk d (Target tgt2)
                        else 
                            return $ State (moveSnake snk (next d snk)) d tgt 

outOfBounds :: Block -> Bool 
outOfBounds (Block (a,b)) = a<0 || b<0 || a >= blocksWide || b >= blocksHigh

victory :: Snake -> Bool 
victory (Snake blks) = blocksWide*blocksHigh == length blks

defeat :: Snake -> Bool 
defeat (Snake blks) = outOfBounds $ head blks

drawBlock :: Block -> Picture
drawBlock (Block (x,y)) = translate (fromIntegral (x*blockSize)) (fromIntegral (y*blockSize)) $ rectangleSolid  (fromIntegral blockSize) (fromIntegral blockSize) 

drawSnake :: Snake -> Picture
drawSnake (Snake blks) = color white $ pictures $ fmap drawBlock blks 

drawTarget :: Target -> Picture 
drawTarget (Target blk) = color red $ drawBlock blk 

drawState :: State -> Picture 
drawState (State snk _ tgt) = pictures [drawSnake snk,drawTarget tgt]

disp :: Display
disp = InWindow "Snake Game" (width,height) (10,10)

main :: IO ()
main = do 
    st <- newState
    playIO disp black 5 st (return.drawState) (\_ g -> return g) (\ _ s -> tickState s)
                               