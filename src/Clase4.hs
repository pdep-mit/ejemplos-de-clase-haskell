module Clase4 where

import Clase2

-----------------------------
-- Parte 1: orden superior
-----------------------------

-- Expresiones lambda
data Persona = Persona {
  nombre :: String,
  edad :: Int
} deriving (Show, Eq)

adolescente :: Persona -> Bool
adolescente persona = edad persona >= 12 && edad persona <= 18

adolescente' :: Persona -> Bool
adolescente' = (\anios -> anios >= 12 && anios <= 18).edad

adolescente'' :: Persona -> Bool
adolescente'' = estaEntre 12 18.edad

estaEntre :: Ord a => a -> a -> a -> Bool
estaEntre inferior superior valor = valor >= inferior && valor <= superior

-- Currificación: lo que hace Haskell por atrás, la base para poder aplicar parcialmente

estaEntre' :: Ord a => a -> (a -> (a -> Bool))
estaEntre' = (\inferior -> (\superior -> (\valor -> valor >= inferior && valor <= superior)))

-- Ejemplos orden superior

-- mayor según longitud / valor absoluto / edad - sin orden superior
elDeMayorLongitud :: String -> String -> String
elDeMayorLongitud string1 string2
  | length string1 > length string2 = string1
  | otherwise = string2

elDeMayorValorAbsoluto :: (Num a, Ord a) => a -> a -> a
elDeMayorValorAbsoluto numero1 numero2
  | abs numero1 > abs numero2 = numero1
  | otherwise = numero2

elDeMayorEdad :: Persona -> Persona -> Persona
elDeMayorEdad persona1 persona2
  | edad persona1 > edad persona2 = persona1
  | otherwise = persona2

-- mayor según longitud / valor absoluto / edad - con orden superior
elDeMayor :: Ord b => (a -> b) -> a -> a -> a
elDeMayor ponderacion x y
  | ponderacion x > ponderacion y = x
  | otherwise = y

-------------------------
---- Práctica
-------------------------

{-
Definir hacerNVeces que dado un número entero, una función y un valor,
evalue esa función sucesivamente sobre el valor, tantas veces como se indique

Ejemplos de uso:
> hacerNVeces 5 not True
False
> hacerNVeces 3 (++ "hola") ""
"holaholahola"
> hacerNVeces 5 (*2) 1
32

-}

hacerNVeces :: Int -> (a -> a) -> a -> a
hacerNVeces 0 f valor = valor
hacerNVeces n f valor | n > 0 = hacerNVeces (n-1) f (f valor)
-- hacerNVeces 3 (++ "hola") ""
-- hacerNVeces 2 (++ "hola") "hola"
-- hacerNVeces 1 (++ "hola") "holahola"
-- hacerNVeces 0 (++ "hola") "holaholahola"

-- Estos no son usos válidos, el dominio está correctamente acotado:
-- hacerNVeces (-2) (++ "hola") ""
-- hacerNVeces 4.5 (++ "hola") ""

-----------------------------
-- Parte 2: listas
-----------------------------

-- Intro listas
type Alimento = String

agregarAlimento :: Alimento -> [Alimento] -> [Alimento]
agregarAlimento alimento listaDeCompras = alimento : listaDeCompras

cantidadDeAlimentos :: [Alimento] -> Int
cantidadDeAlimentos listaDeCompras = length listaDeCompras

yaEstaEnLaLista :: Alimento -> [Alimento] -> Bool
yaEstaEnLaLista alimento listaDeCompras = elem alimento listaDeCompras

-- Listas con recursividad, luego orden superior

todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (numero:numeros)
    = even numero && todosPares numeros

todosAprobados :: [Estudiante] -> Bool
todosAprobados [] = True
todosAprobados (estudiante:estudiantes)
    = aprobo estudiante && todosAprobados estudiantes

-- abstraemos y refactorizamos

todosCumplen :: (a -> Bool) -> [a] -> Bool
todosCumplen criterio [] = True
todosCumplen criterio (x:xs)
   = criterio x && todosCumplen criterio xs

todosPares' :: [Int] -> Bool
todosPares' = todosCumplen even

todosAprobados' :: [Estudiante] -> Bool
todosAprobados' = todosCumplen aprobo

-- y agregamos un tercer uso directamente con todosCumplen
todosCortos :: [String] -> Bool
todosCortos = todosCumplen ((<10).length)

-- Claramente no necesitábamos definir todosCumplen
todosCumplen' :: (a -> Bool) -> [a] -> Bool
todosCumplen' = all

-- Ejemplito con filter
cantidadDePares = length . filter even

----- Fold

{-
Implementaciones de referencia de length y sum

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs
-}

productoria :: Num a => [a] -> a
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- Cómo quedan definidas con foldr

length' :: [a] -> Int
length' lista = foldr (\_ x -> x+1) 0 lista

sum' :: Num a => [a] -> a
sum' numeros = foldr (+) 0 numeros

productoria' :: Num a => [a] -> a
productoria' numeros = foldr (*) 1 numeros

{-
Implementación de referencia de foldr

foldr :: (b -> a -> a) -> a -> [b] -> a
foldr f valor [] = valor
foldr f valor (x:xs) = f x (foldr f valor xs)
-}

-- Ejemplo distinto: maximum

{-
Implementación de referencia de maximum

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs) = x `max`(maximum xs)

Implementación de referencia de foldr1

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f lista = foldr f (last lista) (init lista)
-}

maximum' :: Ord a => [a] -> a
maximum' = foldr1 max



----------------------------
--- Práctica info nutricional
----------------------------

alimentosPocoCaloricos :: [InformacionNutricional] -> [Alimento]
alimentosPocoCaloricos = map alimento . filter pocoCalorico

pocoCalorico = (<=100).calorias

{-
Armar consultas para saber:
- de entre los alimentos que no son poco calóricos,
si hay alguno que tenga más proteínas que grasas

- qué alimento tiene mayor valor calórico, más carbohidratos,
mayor nombre… teniendo en cuenta que ya tenemos esta función:
elDeMayor :: Ord b => (a -> b) -> a -> a -> a
-}

------- Modelo y datos de prueba

data InformacionNutricional = Info {
  alimento :: Alimento,
  calorias :: Int,
  grasas :: Float,
  carbohidratos :: Float,
  proteinas :: Float
} deriving (Show, Eq)

infoManzana = Info "Manzana" 95 0.3 25.1 0.5
infoBanana = Info "Banana" 134 0.5 34.3 1.6
infoPera = Info "Pera" 101 0.2 27.1 0.6
infoEspinaca = Info "Espinaca" 7 0.1 1.1 0.9
infoYogurt = Info "Yogurt" 149 8.0 11.4 8.5
infoGarbanzos = Info "Garbanzos" 269 4.2 45.0 14.5

infosNutricionales = [
  infoManzana, infoBanana, infoPera,
  infoEspinaca, infoYogurt, infoGarbanzos
  ]