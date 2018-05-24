-- Cada planeta tiene un nombre, una posición en el espacio y una relación que indica a cuánto tiempo terrestre equivale pasar un año allí.
data Planeta = UnPlaneta String Posicion (Tiempo -> Tiempo)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta {
  nombre :: String,
  edad :: Tiempo,
  planeta :: Planeta
}

type Velocidad = Float
type Tiempo = Float
type Nave = (Planeta -> Planeta -> Tiempo)

--1a
distanciaEntreDosPlanetas :: Planeta -> Planeta -> Float
--qué pasa si agregamos otra coordenada? eso nos ayuda a identificar la repetición de lógica
--distanciaEntreDosPlanetas (UnPlaneta _ (x1, y1, z1) _) (UnPlaneta _ (x2, y2, z2) _) = sqrt(((x1-x2)^2) + ((y1-y2)^2) + ((z1-z2)^2))
--distanciaEntreDosPlanetas (UnPlaneta _ (x1, y1, z1) _) (UnPlaneta _ (x2, y2, z2) _) = sqrt(diferenciaDeCuadrados x1 x2 + diferenciaDeCuadrados y1 y2 + diferenciaDeCuadrados z1 z2)

-- Planteamos una versión distinta basada en orden superior
diferenciaDeCuadrados a b = (a - b)^2
coordenadas planeta =  map (\funCoord -> (funCoord.posicion) planeta) [coordX, coordY, coordZ]
distanciaEntreDosPlanetas planeta1 planeta2 = (sqrt.sum.zipWith diferenciaDeCuadrados (coordenadas planeta1).coordenadas) planeta2

--1b
tiempoDeViaje :: Velocidad -> Planeta -> Planeta -> Tiempo
tiempoDeViaje velocidad planeta1 planeta2 = (distanciaEntreDosPlanetas planeta1 planeta2) / velocidad

--2
pasarTiempo :: Tiempo -> Astronauta -> Astronauta
pasarTiempo anios astronauta = envejecer ((tiempo.planeta) astronauta anios) astronauta
--tiempo retorna una función!

envejecer :: Tiempo -> Astronauta -> Astronauta
envejecer anios astronauta = astronauta {edad = (edad astronauta) + anios}

--3

viaje :: Nave -> Planeta -> Astronauta -> Astronauta
viaje nave planetaDestino astronauta = (cambiarPlaneta planetaDestino.envejecer (nave (planeta astronauta) planetaDestino)) astronauta

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta nuevoPlaneta astronauta = astronauta {planeta = nuevoPlaneta}

naveVieja :: Int -> Nave
naveVieja tanquesDeOxigeno planetaOrigen planetaDestino
 |tanquesDeOxigeno <= 6 = tiempoDeViaje 10 planetaOrigen planetaDestino
 |otherwise = tiempoDeViaje 7 planetaOrigen planetaDestino

naveFuturista :: Nave
naveFuturista _ _ = 0

--4a
viajeGrupal :: Nave -> Planeta -> [Astronauta] -> [Astronauta]
viajeGrupal nave destino astronautas = map (viaje nave destino) astronautas

rescatar :: Nave -> [Astronauta] -> Astronauta -> [Astronauta]
rescatar nave rescatistas varado = (viajeGrupal nave origen . levantar (pasarTiempo tiempoDestino varado) . viajeGrupal nave destino) rescatistas
  where
    origen = (planeta.head) rescatistas
    destino = planeta varado
    tiempoDestino = nave origen destino

levantar varado rescatistas = varado : rescatistas

--4b
puedeSerRescatado :: Nave -> [Astronauta] -> Astronauta -> Bool
puedeSerRescatado nave rescatistas varado = (any demasiadoViejo.rescatar nave rescatistas) varado

demasiadoViejo = (>90).edad

nombresRescatables :: Nave -> [Astronauta] -> [Astronauta] -> [String]
nombresRescatables nave varados rescatistas = (map nombre.filter (puedeSerRescatado nave rescatistas)) varados
