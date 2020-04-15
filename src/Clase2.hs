module Clase2 where

-- El tipo de f es Bool -> Bool -> Bool
f x y = x && not y

-- El tipo de doble es Num a => a -> a
doble x = x * 2

-- El tipo de identidad es a -> a
-- esta función ya existe en Haskell y se llama id
identidad x = x

-----------------

data Estudiante = UnEstudiante {
    nombre :: String,
    legajo :: String,
    nota :: Int
  } deriving (Show, Eq)

juanita :: Estudiante
juanita = UnEstudiante "Juana" "123456-7" 8

pepito :: Estudiante
pepito = UnEstudiante {
    legajo = "98765-0",
    nombre = "Pepe",
    nota = 6
  }

aprobo :: Estudiante -> Bool
aprobo estudiante = nota estudiante >= 6

legajoYNombre :: Estudiante -> String
legajoYNombre (UnEstudiante legajo nombre _)
  = legajo ++ " " ++ nombre

-- Version con pattern matching
-- leFueMejor (UnEstudiante _ _ mejorNota) (UnEstudiante _ _ otraNota)
--   = mejorNota > otraNota

-- Version sin pattern matching
leFueMejor estudianteConMejorNota otroEstudiante
  = nota estudianteConMejorNota > nota otroEstudiante

--------

cambiarNota :: Int -> Estudiante -> Estudiante
cambiarNota nuevaNota (UnEstudiante nombre legajo _)
  = UnEstudiante nombre legajo nuevaNota

subirNota :: Estudiante -> Estudiante
subirNota estudiante = cambiarNota (nota estudiante + 1) estudiante

---------

truncar :: Int -> String -> (String, Int)
truncar cantidadCaracteres palabra
  = (take cantidadCaracteres palabra, length palabra - cantidadCaracteres)