import qualified Data.Set as Set 

import QuadTree
import qualified Test.QuickCheck as Chk

full :: Rect 
full = ((-1,1),(1,-1))
tl = ((-1,1),(0,0))
tr = ((0,1),(1,0))
bl = ((-1,0),(0,-1))
br = ((0,0),(1,-1))

bounding :: (Point,Point) -> Rect
bounding ((ax,ay),(bx,by)) = ((min ax bx,max ay by),(max ax bx,min ay by))

cmpr :: [P] -> [P] -> Bool
cmpr as bs = Set.fromList as == Set.fromList bs

prop_contained :: (Point,Point) -> (Point,Point) -> [Point] -> Bool 
prop_contained bx treebnds pnts = cmpr (get tree bxb) (fmap P bxpts)
                    where 
                        treb = bounding treebnds
                        bxb = bounding bx
                        tree = foldl (flip insert) (Nil treb) $ fmap P pnts 
                        bxpts = filter (\v -> within v treb) $ filter (\v -> within v bxb) pnts


data P = P Point deriving (Show,Ord,Eq)

instance At P where 
    at (P p) = p 

main = do 
        Chk.quickCheck (Chk.withMaxSuccess 1000 prop_contained)
        Chk.quickCheck (Chk.withMaxSuccess 1000 (prop_contained full full))