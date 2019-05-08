data Meta = UnaMeta {
    peso :: Float, -- en gramos
    pureza :: Float -- 0 a 1, donde 1 es la mejor calidad
} deriving (Show, Eq)

data Personaje = UnPersonaje {
    nombre :: String, nivelIntoxicacion :: Float,
    aguante :: Float, dosisDeMeta :: [Meta], 
    dinero :: Dinero 
} deriving (Show, Eq)

type Dinero = Float

conDinero :: Dinero -> Personaje -> Personaje
conDinero nuevoDinero (UnPersonaje nombre nivelIntoxicacion aguante dosisDeMeta _ ) =
    UnPersonaje nombre nivelIntoxicacion aguante dosisDeMeta nuevoDinero

conNivelIntoxicacion :: Float -> Personaje -> Personaje
conNivelIntoxicacion nuevoNivelIntoxicacion (UnPersonaje nombre _ aguante dosisDeMeta dinero) =
    UnPersonaje nombre nuevoNivelIntoxicacion aguante dosisDeMeta dinero

conAguante :: Float -> Personaje -> Personaje
conAguante nuevoAguante (UnPersonaje nombre nivelIntoxicacion _ dosisDeMeta dinero) =
    UnPersonaje nombre nivelIntoxicacion nuevoAguante dosisDeMeta dinero

conDosisDeMeta :: [Meta] -> Personaje -> Personaje
conDosisDeMeta nuevaDosisDeMeta (UnPersonaje nombre nivelIntoxicacion aguante _ dinero) =
    UnPersonaje nombre nivelIntoxicacion aguante nuevaDosisDeMeta dinero

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun ponderacion x y
    | ponderacion x >= ponderacion y = x
    | otherwise = y

aplicarHasta :: (t -> t) -> (t -> Bool) -> t -> t
aplicarHasta transformacion criterio valor
    | criterio valor = valor
    | otherwise = aplicarHasta transformacion criterio (transformacion valor)

estaDrogado :: Personaje -> Bool
estaDrogado personaje =
    nivelIntoxicacion personaje >= aguante personaje

plusDeAguante :: Personaje -> Meta -> Float
plusDeAguante personaje dosisDeMeta 
    | estaDrogado personaje = pureza dosisDeMeta
    | otherwise = plusDeIntoxicacion dosisDeMeta

plusDeIntoxicacion :: Meta -> Float
plusDeIntoxicacion dosisDeMeta = pureza dosisDeMeta * peso dosisDeMeta

consumirDosis :: Meta -> Personaje -> Personaje
consumirDosis dosisDeMeta personaje =
    aumentarAguante (plusDeAguante personaje dosisDeMeta) .
    aumentarNivelIntoxicacion (plusDeIntoxicacion dosisDeMeta) $ personaje

aumentarNivelIntoxicacion :: Float -> Personaje -> Personaje
aumentarNivelIntoxicacion plusDeNivelIntoxicacion personaje =
  conNivelIntoxicacion
      (plusDeNivelIntoxicacion + nivelIntoxicacion personaje)
      personaje

aumentarAguante :: Float -> Personaje -> Personaje
aumentarAguante plusDeAguante personaje =
    conAguante (plusDeAguante + aguante personaje) personaje

rehabilitar :: Personaje -> Personaje
rehabilitar personaje =
    pasarDiasEnRehabilitacion (perderDosisDeMeta personaje)

pasarDiasEnRehabilitacion :: Personaje -> Personaje
pasarDiasEnRehabilitacion personaje =
    aplicarHasta diaDeRehabilitacion
                 ((<3) . nivelIntoxicacion)
                 personaje

diaDeRehabilitacion :: Personaje -> Personaje    
diaDeRehabilitacion personaje =
    conNivelIntoxicacion (nivelIntoxicacion personaje - 1) .
    conAguante (aguante personaje * 0.75) $ personaje

darselaEnLaPera :: Personaje -> Personaje
darselaEnLaPera personaje =
    foldr consumirDosis
          (perderDosisDeMeta personaje)
          (dosisDeMeta personaje)

perderDosisDeMeta :: Personaje -> Personaje
perderDosisDeMeta personaje = conDosisDeMeta [] personaje

altoNarco :: Personaje -> Bool
altoNarco personaje =
    100000 < (dinero personaje +
              loQuePodriaConseguirVendiendo (dosisDeMeta personaje))
    &&
    5 <= kilosDeLaBuena (dosisDeMeta personaje)

kilosDeLaBuena :: [Meta] -> Float
kilosDeLaBuena =
    gramosAKilos . sum . map peso . filter esDeLaBuena

gramosAKilos gramos = gramos / 1000

esDeLaBuena :: Meta -> Bool
esDeLaBuena unaMeta = pureza unaMeta > 0.95

loQuePodriaConseguirVendiendo :: [Meta] -> Dinero
loQuePodriaConseguirVendiendo = sum . map valorDeLaDosis

valorDeLaDosis :: Meta -> Dinero
valorDeLaDosis (UnaMeta pureza peso) =
    pureza ^ 2 * peso * 10

elegirProveedor :: Ord a =>
                   (Personaje -> a) ->
                   (Meta -> Bool) ->
                   [Personaje] ->
                   Personaje
elegirProveedor ponderacion criterio proveedores =
    foldr1 (mayorSegun ponderacion)
           (soloTienenMetaQue criterio proveedores)

soloTienenMetaQue :: (Meta -> Bool) -> [Personaje] -> [Personaje]
soloTienenMetaQue criterio = filter (soloTieneMetaQue criterio)

soloTieneMetaQue :: (Meta -> Bool) -> Personaje -> Bool
soloTieneMetaQue criterio proveedor =
    all criterio (dosisDeMeta proveedor)

jesse :: Personaje
jesse = UnPersonaje { nombre = "jesse",
                      nivelIntoxicacion = 10,
                      aguante = 500,
                      dosisDeMeta = [
                          UnaMeta 10 1,
                          UnaMeta 1 1
                      ],
                      dinero = 50 }

walterWhite = UnPersonaje { nombre = "walter",
    nivelIntoxicacion = 10,
    aguante = 500,
    dosisDeMeta = [
        UnaMeta 10000000 1,
        UnaMeta 1000000 1
    ],
    dinero = 50 }

jesseDrogado :: Personaje
jesseDrogado = aumentarNivelIntoxicacion 5 jesse
