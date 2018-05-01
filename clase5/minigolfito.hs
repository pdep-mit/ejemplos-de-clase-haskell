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

golpe = undefined

palos = undefined

-- Punto 2
hoyo = undefined
laguna = undefined
tunelConRampita = undefined

puedeSuperar = undefined
superarObstaculo = undefined

-- Punto 3
palosUtiles = undefined

obstaculosConsecutivosSuperables = undefined

paloMasUtil = undefined

-- Punto 4
quienesPierdenLaApuesta = undefined

-- Implementación mentirosa de puntosGanados, que no importa cómo se resuelve
puntosGanados _ (UnJugador "Bart" _ _) = 10
puntosGanados _ (UnJugador "Todd" _ _) = 10
puntosGanados _ (UnJugador "Rafa" _ _) = 1
