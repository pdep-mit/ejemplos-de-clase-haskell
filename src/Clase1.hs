module Clase1 where

doble :: Int -> Int
doble numero = 2 * numero

-- esMayor: dada una edad da true si es mayor o igual a 18
esMayor :: Int -> Bool
esMayor edad = edad >= 18

-- esMenor: lo opuesto a esMayor
esMenor :: Int -> Bool
esMenor edad = not (esMayor edad)

nombreFormateado :: String -> String -> String
nombreFormateado nombre apellido = apellido ++ ", " ++ nombre

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ _ = False

factorial :: Int -> Int
factorial 0 = 1
factorial n
  | n > 0 = n * factorial (n-1)