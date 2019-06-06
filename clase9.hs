module Interestelar where

import Text.Show.Functions

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

-- Notamos que en la fórmula matemática había repetición de lógica,
-- y nos propusimos evitarla
distancia :: Posicion -> Posicion -> Float
distancia pos1 pos2 =
    (sqrt . sum . map (**2) . diferencias'' pos1) pos2

-- De la función diferencias surgieron las siguientes variantes
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

-- La función restarPor no aporta una abstracción muy interesante, difícilmente se use
-- en otro contexto, así que bien podría haber sido una lambda en diferencias'',
-- pero sirvió para pasar de diferencias', donde esa lógica se repetía, a diferencias'' :)
restarPor :: Posicion -> Posicion -> (Posicion -> Float) -> Float
restarPor pos1 pos2 fCoordenada =
    fCoordenada pos1 - fCoordenada pos2

-- Cómo hubiera quedado la función distancia sin toda esta fumeta super interesante,
-- simplemente reescribiendo la fórmula que nos daban en el enunciado:
distancia' (x1,y1,z1) (x2,y2,z2)
  = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2)

-- Es interesante pensar qué impacto tendría en nuestro programa que tengamos que
-- considerar más dimensiones en un futuro, para notar la diferencia de mantenibilidad
-- al usar pattern matching y repetir lógica respecto a la solución que usa diferencias''.


-- 1.b)
tiempoDeViaje :: Planeta -> Float -> Planeta -> Float
tiempoDeViaje planetaOrigen velocidad =
    (/ velocidad) . distanciaEntrePlanetas planetaOrigen

-- 2)
-- Definimos algunas constantes para probar en la consola
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

-- A pedido de la gente, por expresividad
tiempoEquivalente :: Planeta -> (Int -> Int)
tiempoEquivalente = tiempo

pasarTiempo :: Int -> Astronauta -> Astronauta
pasarTiempo anios astronauta =
    envejecer astronauta
    . tiempoEquivalente (planeta astronauta)
    $ anios

-- Abstracción super importante
envejecer :: Astronauta -> Int -> Astronauta
envejecer astronauta anios = astronauta {
    edad = edad astronauta + anios
}

-- 3)
-- Bajamos a Haskell la idea que se menciona en el enunciado sobre qué es una nave:
type Nave = Planeta -> Planeta -> Float

-- Este es el requerimiento que nos pedían resolver:
viajar :: Planeta -> Nave -> Astronauta -> Astronauta
viajar destino nave astronauta =
    cambiarPlaneta destino
    . envejecer astronauta
    . aniosViajandoEn nave astronauta
    $ destino

-- Siguiendo el camino de las abstracciones felices
cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta planeta astronauta = astronauta {
    planeta = planeta
}

aniosViajandoEn nave astronauta =
    aniosViajandoHacia nave (planeta astronauta)

aniosViajandoHacia nave planeta =
    aniosQuePasaron . nave planeta

aniosQuePasaron = floor

-- Estas son las funciones que se pedían para poder obtener naves
naveVieja :: Int -> Nave
naveVieja tanques origen destino =
    tiempoDeViaje origen (velocidadDeNaveVieja tanques) destino

velocidadDeNaveVieja tanques
    | tanques < 6 = 10
    | otherwise = 7

naveFuturista :: Nave
naveFuturista _ _ = 0

{-
Ejemplos de uso:
*Interestelar> viajar laTierra (naveVieja 7) beto
UnAstronauta {nombre = "Roberto", edad = 72, planeta = UnPlaneta {nombrePlaneta = "La Tierra", posicion = (0.0,0.0,0.0), tiempo = <function>}}

*Interestelar> viajar laTierra (naveVieja 5) beto
UnAstronauta {nombre = "Roberto", edad = 55, planeta = UnPlaneta {nombrePlaneta = "La Tierra", posicion = (0.0,0.0,0.0), tiempo = <function>}}

*Interestelar> viajar laTierra naveFuturista beto
UnAstronauta {nombre = "Roberto", edad = 14, planeta = UnPlaneta {nombrePlaneta = "La Tierra", posicion = (0.0,0.0,0.0), tiempo = <function>}}
-}


-- 4.a)
-- Este es un problema complejo, así que hay que ordenar bien el problema.
-- También hace que quede más entendible usar definiciones locales (usando where).
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

-- Expresividad :D
agregarALaTripulacion = (:)

viajeGrupal :: Nave -> Planeta -> [Astronauta] -> [Astronauta]
viajeGrupal nave destino astronautas =
    map (viajar destino nave) astronautas

-- 4.b)

nombresDeLosQuePuedenSerRescatados :: [Astronauta] -> Nave -> [Astronauta] -> [String]
nombresDeLosQuePuedenSerRescatados rescatistas nave =
    map nombre . filter (puedeRescatarse rescatistas nave)


puedeRescatarse :: [Astronauta] -> Nave -> Astronauta -> Bool
puedeRescatarse rescatistas nave varado =
    (all (not . esViejo) . rescatar rescatistas nave) varado

esViejo :: Astronauta -> Bool
esViejo = (>90) . edad

---
--- And now, for something completely different...
---

f a b c = any ((> b).a b).filter (c 10)

-- El tipo inferido en clase fue:
-- f :: Ord x => (x -> y -> x) -> x -> (Int -> y -> Bool) -> [y] -> Bool
-- que mencionamos que podía también generalizarse un poco más cambiando Int por Num z => z,
-- ya que en Haskell, 10 :: Num t => t.
-- f :: (Ord x, Num z) => (x -> y -> x) -> x -> (z -> y -> Bool) -> [y] -> Bool

{-
¿Cómo llegamos a eso?

1) Arrancamos por el viejo truco de poner 3 flechas -> por cada parámetro explícito,
para ordenarnos y poder encarar el problema de a poquito.
Luego agregamos una flecha más porque notaron enseguida que estaba definida point-free
en base a una composición de funciones.
Repasamos que como las funciones están currificadas, esto:
??? -> ??? -> ??? -> ??? -> ???
Es equivalente a esto otro:
??? -> ??? -> ??? -> (??? -> ???)

2) Notaron que tanto a como c tenían que ser funciones, porque estaban siendo aplicadas.
  2.1) El tipo de c tenía que recibir primero un número, y luego un parámetro más y retornar Bool
  porque se está usando (c 10) como parámetro de filter, que va a usar esa función con cada elemento
  de la lista a filtrar. Por eso si tuviéramos que decir de qué tipo es c, más allá de las otras cosas,
  sería Num t1 => t1 -> t2 -> Bool, porque sobre t2 no sabemos más nada en este contexto.
  2.2) El tipo de a tenía que recibir primero algo del mismo tipo de b, y luego un parámetro más,
  porque se está componiendo (a b) con (> b), entonces (a b) tiene que ser una función porque
  sólo las funciones se pueden componer. A su vez, debido a esa composición, el tipo de retorno
  de la función a debía ser el mismo tipo de b, para que puedan ser comparados con (>).
  El tipo del segundo parámetro de a no está relacionado con lo que recibe como primer parámetro,
  ni con lo que retorna, por eso si tuviéramos que decir de qué tipo es a, más allá de las otras cosas,
  sería Ord t1 => t1 -> t2 -> t1.

3) Respecto al tipo de b, lo único que lo restringe es su uso con la función (>), por eso le pusimos
una letra cualquiera, en este caso x, y agregamos la restricción Ord x => ..., ya que sólo los valores de
tipos para los cuales existe una instancia de la typeclass Ord pueden usarse como parámetros de (>).

4) El hecho de que el último parámetro de f esa una lista y que el tipo de retorno sea Bool
se desprende de la composición de funciones de más afuera:
  4.1) Sabemos que el tipo de filter es (t -> Bool) -> [t] -> [t], y ya está aplicado con el criterio
  de filtrado que no nos restringe de ninguna forma qué es el tipo t por lo analizado en el 2.1),
  por ende la función de más a la derecha de la composión es de tipo [t] -> [t].
  4.2) Sabemos que el tipo de any es (t -> Bool) -> [t] -> Bool, y ya está aplicado con el criterio,
  por ende la función de más a la izquierda de la composión es de tipo [t] -> Bool. Nuevamente, t
  podría ser cualquier cosa, porque el criterio que recibe que es ((> b).a b), que por lo analizado
  en el 2.2) es una composición entre (> b) :: Ord t1 => t1 -> Bool y (a b) :: Ord t1 => t1 -> t2 -> t1,
  o sea que la función compuesta ((> b).a b) :: t2 -> Bool.
  4.3) Eso significa que toda la composión es de tipo [t] -> Bool.

5) Como no había ninguna restricción sobre cuál era el tipo de los elementos de la lista,
le pusimos una letra cualquiera (en este caso quedó y), ya que los elementos de la lista se usan
únicamente como parámetro de las funciones a y c que ya determinamos que no restringen el tipo
de ese parámetro, no hay nada más que estemos pasando por alto.

Poniendo todo eso junto, podemos llegar al tipo que infiere Haskell si no le decimos de qué tipo es f:
*Interestelar> :t f
f :: (Ord t1, Num t2) =>
     (t1 -> a -> t1) -> t1 -> (t2 -> a -> Bool) -> [a] -> Bool

https://i.kym-cdn.com/photos/images/newsfeed/000/517/111/fbd.jpg
-}
