module Clase6 where

import Clase1

ignorar :: a -> b -> a
ignorar x _ = x

enterosDesde :: Int -> [Int]
enterosDesde x = x : enterosDesde (x + 1)

-- Números primos

esDivisorDe :: Int -> Int -> Bool
esDivisorDe multiplo divisor = multiplo `mod` divisor == 0

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = all (not . esDivisorDe n) [2..(n-1)]

primos :: [Int]
primos = filter esPrimo [1..]

-- Práctica

{-
Saga de películas:
Queremos generar, a partir del título de una película,
los títulos de la saga completa.
A cada secuela se le debe agregar el número de película de la saga.
-}

saga :: String -> [String]
saga titulo = titulo : map (\n -> titulo ++ " " ++ show n) [2 ..]
saga' titulo = titulo : map ((titulo ++).(" " ++).show) [2 ..]







{-
Enésimo Avatar:
Sabiendo que existe una secuencia cíclica de elementos relacionada
con el origen de los avatar: Tierra, Fuego, Aire, Agua…
Queremos saber el elemento natural del enésimo avatar.
-}

type ElementoNatural = String
enesimoAvatar :: Int -> ElementoNatural
enesimoAvatar n = cicloDelAvatar !! (n-1)

cicloDelAvatar :: [ElementoNatural]
cicloDelAvatar = cycle ["Tierra", "Fuego", "Aire", "Agua"]