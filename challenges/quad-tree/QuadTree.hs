module QuadTree (QuadTree(Nil),get,insert,Point,Rect,At(at),within,toList,fromList) where 

type Point =(Float,Float)
type Rect = (Point,Point) 

class At a where 
    at :: a -> Point 

data QuadTree a = Nil Rect | Sub Rect a [QuadTree a]

fracture:: Rect -> [Rect] 
fracture ((lx,ty),(rx,by)) = [((lx,ty),(mx,my)),
                             ((mx,ty),(rx,my)),
                             ((lx,my),(mx,by)),
                             ((mx,my),(rx,by))]
                    where 
                        mx = (lx+rx)/2
                        my = (ty+by)/2


within :: Point -> Rect -> Bool
within (x,y) ((lx,ty),(rx,by)) = x >= lx && x < rx && y >= by &&  y < ty

intersect :: Rect -> Rect -> Bool 
intersect ((alx,aty),(arx,aby)) ((blx,bty),(brx,bby)) = not clear 
                    where
                        above = (aby > bty) -- a above b
                        below = (bby > aty) -- a below b 
                        left = (blx > arx) -- a left of b 
                        right = (alx > brx) -- a right of b 
                        clear = (above || below || left || right)
                        
bounds :: QuadTree a -> Rect 
bounds (Nil b) = b 
bounds (Sub b _ _) = b 

get :: At a => QuadTree a -> Rect -> [a]
get (Nil _) rect = []
get (Sub b a chld) rect = root ++ (concatMap sub chld)
                where 
                    root = if within (at a) rect then [a] else [] 
                    sub x = if intersect rect (bounds x) then get x rect else []

insert :: At a => a -> QuadTree a -> QuadTree a 
insert v (Sub b a chld) = if within (at v) b then Sub b a (fmap (insert v) chld) else Sub b a chld
insert v (Nil b) = if within (at v) b then Sub b v (fmap Nil $ fracture b) else Nil b

toList :: QuadTree a -> [(Rect,a)]
toList (Nil _) = []
toList (Sub b a chld) = [(b,a)] ++ concatMap toList chld

fromList :: At a => Rect -> [a] -> QuadTree a 
fromList r pts = foldl (flip insert) (Nil r) pts 





