--Recursividad
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--Restringimos más el dominio
fibonacci' 0 = 1
fibonacci' 1 = 1
fibonacci' n
 | n > 1 = fibonacci' (n - 1) + fibonacci' (n - 2)
 
--Algunos tipos
--(++) :: [a] -> [a] -> [a]
--take :: Int -> [a] -> [a]
--length :: [a] -> Int

floca1 :: [Int] -> [Int]
floca1 = ([1,2,3]++)

--Listas
--funcion [] = ...
--funcion [x] = ... lista de un unico elemento
--funcion (cabeza:cola) = ... lista de AL MENOS un elemento
--funcion (cabeza1:cabeza2:cola) = ... lista de AL MENOS dos elemento
--cabeza :: a, cola :: [a]

--[1,2,3] es equivalente a (1:(2:(3:[])))

-- (:) :: a -> [a] -> [a]

floca2 :: [[Int]] -> [[Int]]
floca2 = ([1,2]:)

--cuentaRegresivaDesde 3 -> [3,2,1,0]
cuentaRegresivaDesde 0 = [0]
cuentaRegresivaDesde n = n:cuentaRegresivaDesde (n-1)

--Rangos
--[0..10] = [0,1,2,3,4,5,6,7,8,9,10]
cuentaRegresivaDesde' n = reverse [0..n]

--sumarATodos 1 [1,2,3] -> [2,3,4]
sumarATodos :: Int -> [Int] -> [Int]
sumarATodos _ [] = []
sumarATodos n (x:xs) = n + x : sumarATodos n xs

restarATodos :: Int -> [Int] -> [Int]
restarATodos _ [] = []
restarATodos n (x:xs) = (x - n) : restarATodos n xs 

--Pero en estos 2 casos estamos REPITIENDO LÓGICA!!!

--Orden superior => Función que recibe una función como parámetro

transformarATodos :: (a -> b) -> [a] -> [b]
transformarATodos _ [] = []
transformarATodos unaFuncion (x:xs) = unaFuncion x : transformarATodos unaFuncion xs

sumarATodos' n lista = transformarATodos (n+) lista
restarATodos' n lista = transformarATodos (\x -> x - n) lista

--TransformarATodos ya existe y se llama map

--map, composicion (.), aplicacion ($)
-- (.) :: (c -> b) -> (a -> c) -> (a -> b)
-- ($) :: (a -> b) -> a -> b 

--otra funcion importante: filter
-- filter :: (a -> Bool) -> [a] -> [a]

pares :: [Int] -> [Int]
pares = filter even

-- all/any :: (a -> Bool) -> [a] -> Bool
-- foldl :: (b -> a -> b) -> b -> [a] -> b

productoria lista = foldl (*) 1 lista

--con fold
all' criterio lista = foldl (\todos elem -> todos && criterio elem) True lista

--generando una lista de booleanos
all'' criterio lista = foldl (&&) True (map criterio lista)

--usando como semilla el primer elemento de la lista con foldl1
--foldl1 :: (a -> a -> a) -> [a] -> a, que restringe mucho más

all''' criterio lista = foldl1 (&&) (map criterio lista)

--foldl (*) 1 [1..5] ((((1*1)2*)3*)4*)*5
--foldr (*) 1 [1..5] 1*(2*(3*(4*(5*1))))

--parcial gravity falls
-- zipWithIf (+) even [1,2,3] [3,4,5,6] -> [3, 4+1, 5, 6+2]

zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = []
zipWithIf f criterio (x:xs) (y:ys)
 | criterio y = f x y : zipWithIf f criterio xs ys --Avanzo en las 2 listas
 | otherwise = y:zipWithIf f criterio (x:xs) ys --Avanzo solamente la segunda lista