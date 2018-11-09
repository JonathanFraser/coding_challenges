module Planets where




data Orbit = Orbit {minorRadius :: Float, majorRadius:: Float, rotation:: Float}
data Category = Star | GasGiant | Earthlike | Moon 
data Body = Body {category :: Category, radius:: Float }
data System = System Body [(Orbit,System)]


location :: Orbit -> Float -> (Float,Float)
location orb phase = (x',y') 
                where 
                    x = majorRadius orb * cos phase 
                    y = minorRadius orb * sin phase 
                    r = rotation orb 
                    x' = (cos r) * x - (sin r) * y 
                    y' = (cos r) * y + (sin r) * x 

mass :: Body -> (Category -> Float) -> Float 
mass body density = (4/3*pi*(radius body)**3) * density (category body) 
