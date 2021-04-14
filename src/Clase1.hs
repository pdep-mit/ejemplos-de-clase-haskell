module Clase1 where

doble :: Int -> Int
doble numero = 2 * numero

-- esMayor: dada una edad da true si es mayor o igual a 18
esMayor :: Int -> Bool
esMayor edad = edad >= 18

-- esMenor: lo opuesto a esMayor
esMenor :: Int -> Bool
esMenor edad = not (esMayor edad)

-- nombreFormateado: dado un nombre y un apellido, retorna el nombre completo con el formato: Apellido, Nombre
nombreFormateado :: String -> String -> String
nombreFormateado nombre apellido = apellido ++ ", " ++ nombre

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ _ = False

-- n == 0 -> 1, n > 0 -> n * !n-1
-- Le pusimos Integer en vez de Int para que funcione bien con números grandes, no es importante
factorial :: Integer -> Integer
factorial 0 = 1
factorial n
  | n > 0 =  n * factorial (n-1)