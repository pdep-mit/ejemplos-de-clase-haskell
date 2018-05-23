-- Cada planeta tiene un nombre, una posición en el espacio y una relación que indica a cuánto tiempo terrestre equivale pasar un año allí.
data Planeta = UnPlaneta String Posicion (Int -> Int)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta String Int Planeta

nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p
