doble :: Int -> Int
doble numero = 2 * numero

esMultiploDe multiplo numero = 
  mod multiplo numero == 0

esMayor :: Int -> Bool
esMayor edad = edad >= 18

saludar :: String -> String
saludar nombre = "Hola " ++ nombre

xorAire :: Bool -> Bool -> Bool
xorAire p q = p && not q || not p && q
xorAgua :: Bool -> Bool -> Bool
xorAgua p q = p /= q
xorTierra :: Bool -> Bool -> Bool
xorTierra b1 b2 =
  not (b1 == b2)



fLoca1 :: Int -> Bool -> Bool
fLoca1 a b = a > 5 || b

fLoca2 :: a -> Int -> Int
fLoca2 a b = b + 1

fLoca3 :: Int -> Int -> Bool
fLoca3 a b = min a 10 > b

fLoca4 :: a -> b -> b
fLoca4 n m = m

intermedio :: Int -> Int -> Int -> Int
intermedio a b c = 
  (min a b) `max` (min b c) `max` (min a c) 

intermedio' :: Int -> Int -> Int -> Int
intermedio' a b c = 
  sumaTotal - minimo - maximo
  where
    sumaTotal = a + b + c
    minimo = (min a (min b c))
    maximo = (max a (max b c))

numeroMagico = 10