module Minigolfito where
-- Enunciado:
-- https://docs.google.com/document/d/1DoAh_2jGWL5jPeM0K0lQ4V-8MEtqmKV1c3d9eqc9SJQ/

-- Modelo inicial: Jugadores
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = UnaHabilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

-- Funciones útiles
between :: (Enum a, Eq a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord b => (a->b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord b => (a->b) -> a -> a -> a
mayorSegun f x y
  | f x >= f y = x
  | otherwise = y

-- Resolución del ejercicio:
-- Punto 1
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (precisionJugador habilidad * 2) 0
madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad `div` 2) 5
hierro :: Int -> Palo
hierro n habilidad | between 1 10 n =
  UnTiro { velocidad = n * fuerzaJugador habilidad,
           precision = precisionJugador habilidad `div` n,
           altura = max 0 (n - 3) }

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

palos :: [Palo]
palos = [madera, putter] ++ palosDeHierro

palosDeHierro :: [Palo]
palosDeHierro = map hierro [1..10]

-- Punto 2
--type Obstaculo = Tiro -> Tiro

instance Show Obstaculo where
  show = nombreObstaculo

instance Eq Obstaculo where
  unObstaculo == otroObstaculo =
    nombreObstaculo unObstaculo == nombreObstaculo otroObstaculo

data Obstaculo = UnObstaculo { nombreObstaculo :: String,
                               puedeSuperar :: Tiro -> Bool,
                               tiroResultante :: Tiro -> Tiro
                             }

hoyo :: Obstaculo
hoyo = obstaculo "hoyo" puedeSuperarHoyo (\_ -> (UnTiro 0 0 0))

puedeSuperarHoyo tiro = between 5 20 (velocidad tiro) &&
                          (precision tiro) >  95 && (altura tiro) == 0

laguna :: Int -> Obstaculo
laguna largo =
  obstaculo "laguna" puedeSuperarLaguna (tiroResultanteLaguna largo)

tiroResultanteLaguna largo tiro =
  (UnTiro (velocidad tiro) (precision tiro) (altura tiro `div` largo))
puedeSuperarLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

obstaculo :: String -> (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
obstaculo = UnObstaculo

tunelConRampita :: Obstaculo
tunelConRampita = obstaculo "tunel con rampita" puedeSuperarTunelConRampita (\tiro -> (UnTiro { velocidad = velocidad tiro * 2, precision = 100, altura = 0}))
puedeSuperarTunelConRampita tiro = precision tiro > 90 && altura tiro == 0

superarObstaculo = tiroResultante

-- Punto 3
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo =
  filter (puedeSuperar obstaculo . golpe jugador) palos

  -- (\palo -> puedeSuperar obstaculo (golpe jugador palo))
  -- puedeSuperar obstaculo == f
  -- golpe jugador == g
  -- palo == x
  -- (\x -> f (g x)) === (f . g)

obstaculosConsecutivosSuperables :: Tiro -> [Obstaculo] ->  [Obstaculo]  
obstaculosConsecutivosSuperables _ [] = []
obstaculosConsecutivosSuperables tiro (obstaculo:obstaculos)
  | puedeSuperar obstaculo tiro =
      obstaculo : obstaculosConsecutivosSuperables 
                    (tiroResultante obstaculo tiro) obstaculos
  | otherwise = []

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (utilidad jugador obstaculos) palos

utilidad jugador obstaculos palo  = 
  (length . obstaculosConsecutivosSuperables (golpe jugador palo)) obstaculos
-- Punto 4
quienesPierdenLaApuesta = undefined

-- Implementación mentirosa de puntosGanados, que no importa cómo se resuelve
puntosGanados _ (UnJugador "Bart" _ _) = 10
puntosGanados _ (UnJugador "Todd" _ _) = 10
puntosGanados _ (UnJugador "Rafa" _ _) = 1
