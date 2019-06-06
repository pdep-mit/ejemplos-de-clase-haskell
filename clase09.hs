module Interestelar where

import Text.Show.Functions
import Data.List

-- Cada planeta tiene un nombre, una posición en el espacio y una relación que indica a cuánto tiempo terrestre equivale pasar un año allí.
data Planeta = UnPlaneta {
    nombrePlaneta :: String,
    posicion :: Posicion,
    tiempo :: Tiempo
} deriving Show

type Tiempo = Int -> Int

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta {
    nombre :: String,
    edad :: Int,
    planeta :: Planeta
} deriving Show

-- 1.a)
distanciaEntrePlanetas :: Planeta -> Planeta -> Float
distanciaEntrePlanetas planeta = 
    distancia (posicion planeta) . posicion


distancia :: Posicion -> Posicion -> Float
distancia pos1 pos2 =
    (sqrt . sum . map (**2) . diferencias' pos1) pos2

diferencias :: Posicion -> Posicion -> [Float]
diferencias (x1, y1, z1) (x2, y2, z2) = 
    [x1 - x2, y1 - y2, z1 - z2]


diferencias' :: Posicion -> Posicion -> [Float]
diferencias' pos1 pos2 = 
    [
        restarPor pos1 pos2 coordX, 
        restarPor pos1 pos2 coordY, 
        restarPor pos1 pos2 coordZ 
    ]

diferencias'' :: Posicion -> Posicion -> [Float]
diferencias'' pos1 pos2 =
    map (restarPor pos1 pos2) [coordX, coordY, coordZ]
    
restarPor :: Posicion -> Posicion -> (Posicion -> Float) -> Float
restarPor pos1 pos2 fCoordenada =
    fCoordenada pos1 - fCoordenada pos2


-- 1.b)
tiempoDeViaje :: Planeta -> Float -> Planeta -> Float
tiempoDeViaje planetaOrigen velocidad =
    (/ velocidad) . distanciaEntrePlanetas planetaOrigen


xz202 = UnPlaneta {
    nombrePlaneta = "xz202",
    posicion = (20, 100, 400),
    tiempo = (*2)
}

laTierra = UnPlaneta {
    nombrePlaneta = "La Tierra",
    posicion = (0, 0, 0),
    tiempo = id
}

beto = UnAstronauta {
    nombre = "Roberto",
    edad = 14,
    planeta = xz202
}

carla = UnAstronauta {
    nombre = "Carla",
    edad = 24,
    planeta = laTierra
}

tiempoEquivalente :: Planeta -> (Int -> Int)
tiempoEquivalente = tiempo

pasarTiempo :: Int -> Astronauta -> Astronauta
pasarTiempo anios astronauta =
    envejecer astronauta 
    . tiempoEquivalente (planeta astronauta) 
    $ anios

envejecer :: Astronauta -> Int -> Astronauta
envejecer astronauta anios = astronauta {
    edad = edad astronauta + anios
}

-- 3)


viajar :: Planeta -> Nave -> Astronauta -> Astronauta
viajar destino nave astronauta =
    cambiarPlaneta destino 
    . envejecer astronauta 
    . aniosViajandoEn nave astronauta
    $ destino

    
cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta planeta astronauta = astronauta {
    planeta = planeta
}


aniosViajandoEn nave astronauta = 
    aniosViajandoHacia nave (planeta astronauta) 

aniosViajandoHacia nave planeta = 
    aniosQuePasaron . nave planeta
    
    

aniosQuePasaron = floor

type Nave = Planeta -> Planeta -> Float

naveVieja :: Int -> Nave
naveVieja tanques origen destino =
    tiempoDeViaje origen (velocidadDeNaveVieja tanques) destino

velocidadDeNaveVieja tanques
    | tanques < 6 = 10
    | otherwise = 7

naveFuturista :: Nave
naveFuturista _ _ = 0


-- 4.a)

rescatar :: [Astronauta] -> Nave -> Astronauta -> [Astronauta]
rescatar rescatistas nave varado =
    viajeGrupal nave origen
    . agregarALaTripulacion (pasarTiempo tiempoDeViaje varado)
    . viajeGrupal nave destino 
    $ rescatistas
    where
        origen = planeta (head rescatistas)
        destino = planeta varado
        tiempoDeViaje = aniosViajandoHacia nave origen destino


agregarALaTripulacion = (:)

viajeGrupal :: Nave -> Planeta -> [Astronauta] -> [Astronauta]
viajeGrupal nave destino astronautas =
    map (viajar destino nave) astronautas



-- 4.b)

puedenSerRescatados :: [Astronauta] -> Nave -> [Astronauta] -> [Astronauta]
puedenSerRescatados rescatistas nave varados =
    filter (puedeRescatarse rescatistas nave) varados


puedeRescatarse :: [Astronauta] -> Nave -> Astronauta -> Bool
puedeRescatarse rescatistas nave varado =
    (all (not . esViejo) . rescatar rescatistas nave) varado

esViejo :: Astronauta -> Bool
esViejo = (>90) . edad