-- Enunciado:
-- https://docs.google.com/document/d/1DoAh_2jGWL5jPeM0K0lQ4V-8MEtqmKV1c3d9eqc9SJQ/

-- Modelo inicial: Jugadores
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerza :: Int,
  precision :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad {fuerza = 25, precision = 60})
todd = UnJugador "Todd" "Ned" (Habilidad {fuerza = 15, precision = 80})
rafa = UnJugador "Rafa" "Gorgory" (Habilidad {fuerza = 10, precision = 1})

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a >= f b = a
  | otherwise = b

-- Resolución del ejercicio:
