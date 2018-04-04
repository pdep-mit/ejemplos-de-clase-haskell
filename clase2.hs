--Ejercicios resueltos

dispersion :: (Num a, Ord a) => a -> a-> a-> a
dispersion d1 d2 d3 = max d1 (max d2 d3) - min d1 (min d2 d3)

esCuadradoPerfecto nro = decimalPart (sqrt nro) == 0

decimalPart :: Float -> Integer 
decimalPart f = read (tail (tail (show (f)))) :: Integer

--Clase del 04/04/18

cuartoDeLibra :: Int 
cuartoDeLibra = 170

agrandar :: Int -> Int
agrandar precio = precio + 5

agregar :: String -> Int -> Int 
agregar ingrediente precio = precio + length ingrediente

descontar :: Int -> Int -> Int 
descontar porcentaje precio = precio - (precio * porcentaje `div` 100)

--Vamos a pedir una hamburguesa!
--1) agrandamos
--2) agrandamos
--3) agregamos zanahoria
--4) agregamos remolacha
--5) descontar 20

--Esto hizo llorar a Bob Esponja por 10hs seguidas:
--descontar 20 (agregar "remolacha" (agregar "zanahoria" (agrandar (agrandar cuartoDeLibra)))) 
--Esto está mucho mejor:
--((descontar 20).(agregar "remolacha").(agregar "zanahoria").agrandar.agrandar) cuartoDeLibra

-- Usando COMPOSICIÓN y APLICACIÓN PARCIAL tenemos una forma de trabajar más linda y poderosa:
comboJuanizado :: Int -> Int
comboJuanizado = descontar 20.agregar "remolacha".agregar "zanahoria".agrandar.agrandar

-- ¿Cuáles son Funciones en comboJuanizado?:
--descontar
--agregar
--agrandar
--descontar 20
--agregar "remolacha"
--agregar "zanahoria"
-- descontar 20.agregar "remolacha".agregar "zanahoria".agrandar.agrandar
-- .

--Otras versiones de funciones usando aplicación parcial y composición:
descontar' porcentaje precio = ((precio -).(`div` 100).(precio  *)) porcentaje

esCuadradoPerfecto' = (==0).decimalPart.sqrt

dispersion' d1 d2 d3 = (max d1.max d2) d3 - (min d1.min d2) d3