-- Enunciado:
-- https://docs.google.com/document/d/1DoAh_2jGWL5jPeM0K0lQ4V-8MEtqmKV1c3d9eqc9SJQ/
module Minigolfito where

-- Modelo inicial: Jugadores
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a >= f b = a
  | otherwise = b

-- Resolución del ejercicio:
-- Punto 1

putter = undefined
madera = undefined
hierro = undefined

-- golpe :: Jugador -> Palo -> Tiro
golpe = undefined

-- palos :: [Palo]
palos = undefined

-- Punto 2
-- Jugadores de ejemplo (para probar por consola)
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)
