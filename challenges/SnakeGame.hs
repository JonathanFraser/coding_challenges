import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as Set 
import qualified System.Random as Rand 
import qualified Control.Monad.Random as MRand

blockSize = 10 
blocksWide = 40
blocksHigh = 40

width =  (blocksWide*blockSize)
height =  (blocksHigh*blockSize)

data Block = Block (Int,Int) deriving (Eq,Ord)

data Snake = Snake [Block]
data Target = Target Block

data Condition = Start | InPlay | Victory | Loss 
data Dir = DirUp | DirDown | DirLeft | DirRight
data State = State Condition Snake Dir Target

fullSet :: Set.Set Block 
fullSet = Set.fromList [Block (x,y) | x <- [0..(blocksWide-1)], y <- [0..(blocksHigh-1)]]

next :: Dir -> Snake -> Block 
next DirUp (Snake blks) = Block (a,b+1) where Block (a,b) = head blks
next DirDown (Snake blks) = Block (a,b-1) where Block (a,b) = head blks
next DirLeft (Snake blks) = Block (a-1,b) where Block (a,b) = head blks
next DirRight (Snake blks) = Block (a+1,b) where Block (a,b) = head blks

growSnake :: Snake -> Block -> Snake 
growSnake (Snake blks) next = Snake (next:blks) 

moveSnake :: Snake -> Block -> Snake 
moveSnake (Snake blks) next = Snake (next:(init blks))

selfCollide :: Snake -> Dir -> Bool 
selfCollide (Snake blks) d = elem (next d (Snake blks)) blks 

collide :: Snake -> Dir -> Target -> Bool
collide snk dir (Target tgt) = tgt == next dir snk 

newBlock :: [Block] -> IO Block
newBlock snk = do 
                let rem = Set.difference fullSet (Set.fromList snk)
                let pts = zip (Set.toList rem) (repeat 1)
                Rand.getStdRandom $ MRand.runRand (MRand.fromList pts) 


newState :: IO State 
newState = do 
          let snk = Block (quot blocksWide 2,quot blocksHigh 2)
          tgt <- newBlock [snk]
          return $ State Start (Snake [snk]) DirLeft (Target tgt) 


tickState :: State -> IO State 
tickState (State InPlay snk d tgt) = if collide snk d tgt then 
                            do
                                let bsnk = growSnake snk (next d snk)
                                let (Snake blks) = bsnk
                                tgt2 <- newBlock blks
                                return $ State InPlay bsnk d (Target tgt2)
                        else 
                            if defeat snk d then
                                return $ State Loss snk d tgt 
                            else 
                                if victory snk then 
                                    return $ State Victory snk d tgt
                                else 
                                    return $ State InPlay (moveSnake snk (next d snk)) d tgt 
tickState (State a snk d tgt) = return $ State a snk d tgt



eventState :: Event -> State -> IO State 
eventState (EventKey (SpecialKey KeyDown) Down  _ _) (State InPlay snk dir tgt) = return $ State InPlay snk DirDown tgt 
eventState (EventKey (SpecialKey KeyUp) Down  _ _) (State InPlay snk dir tgt) = return $ State InPlay snk DirUp tgt
eventState (EventKey (SpecialKey KeyLeft) Down  _ _) (State InPlay snk dir tgt) = return $ State InPlay snk DirLeft tgt
eventState (EventKey (SpecialKey KeyRight) Down  _ _) (State InPlay snk dir tgt) = return $ State InPlay snk DirRight tgt
eventState (EventKey (SpecialKey KeyEnter) Down _ _) (State Start snk dir tgt) = return $ State InPlay snk dir tgt
eventState (EventKey (SpecialKey KeyEnter) Down _ _) (State Victory snk dir tgt) = newState
eventState (EventKey (SpecialKey KeyEnter) Down _ _) (State Loss snk dir tgt) = newState
eventState _ s = return $ s 

outOfBounds :: Block -> Bool 
outOfBounds (Block (a,b)) = a<0 || b<0 || a >= blocksWide || b >= blocksHigh

victory :: Snake -> Bool 
victory (Snake blks)  = blocksWide*blocksHigh == length blks

defeat :: Snake -> Dir -> Bool 
defeat  (Snake blks) d = (outOfBounds $ head blks) || (selfCollide (Snake blks) d)

drawBlock :: Block -> Picture
drawBlock (Block (x,y)) = translate (fromIntegral (x*blockSize)) (fromIntegral (y*blockSize)) $ rectangleSolid  (fromIntegral blockSize) (fromIntegral blockSize) 

drawSnake :: Snake -> Picture
drawSnake (Snake blks) = color white $ pictures $ fmap drawBlock blks 

drawTarget :: Target -> Picture 
drawTarget (Target blk) = color red $ drawBlock blk 


drawBoard :: State -> Picture 
drawBoard (State _ snk _ tgt) = translate (-(fromIntegral width)/2 + 5) (-(fromIntegral height)/2 + 5) $ pictures [drawSnake snk,drawTarget tgt]

drawVictory :: Picture 
drawVictory =  color green $ translate (-50) 0 $ color white $ scale 0.1 0.1 $ text "Victory!"

drawDefeat :: Picture
drawDefeat =  color red $ translate (-50) 0 $ color white $ scale 0.1 0.1 $ text "You Fail!"

draw2start :: Picture 
draw2start = translate (-50) 0 $ color white $ scale 0.1 0.1 $ text "Enter to Start"

drawState :: State -> Picture 
drawState (State InPlay snk d tgt) = drawBoard (State InPlay snk d tgt)
drawState (State Start _ _ _) = draw2start
drawState (State Victory snk d tgt) = pictures [drawBoard (State Victory snk d tgt), drawVictory]
drawState (State Loss snk d tgt) = pictures [drawBoard (State Loss snk d tgt), drawDefeat]


disp :: Display
disp = InWindow "Snake Game" (width,height) (10,10)

main :: IO ()
main = do 
    st <- newState
    playIO disp black 10 st (return.drawState) eventState (\ _ s -> tickState s)
                               