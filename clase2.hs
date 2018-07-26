-- Ejercicios resueltos aplicando lo visto hasta la clase pasada

-- dispersion: toma tres mediciones y devuelve la diferencia entre la más alta y la más baja
dispersion :: (Num a, Ord a) => a -> a-> a-> a
dispersion d1 d2 d3 = max d1 (max d2 d3) - min d1 (min d2 d3)

-- esCuadradoPerfecto: se cumple si el número recibido es cuadrado de algún número. Eso se cumple si
-- la raíz cuadrada del número tiene como parte decimal el valor 0
-- esta función ya viene dada en el ejercicio, sólo hay que usarla:
decimalPart x = x - fromIntegral (truncate x)

esCuadradoPerfecto nro = decimalPart (sqrt nro) == 0

-----------------------
-- Clase del 04/04/18
-----------------------

{-
Nos pusimos una hamburguesería y queremos implementar un sistema para calcular los precios de los combos que vamos a vender.
Cada combo tiene un precio base que puede ir cambiando según modificaciones que se hacen en el mismo como por ejemplo:
- Agrandar: cada vez que se agranda un combo, el precio aumenta 5 pesos.
- Agregar: recibe el nombre de un ingrediente y aumenta el precio en 1 por cada carácter en el nombre del ingrediente.
- Descuento: recibe el precio, y el porcentaje de descuento, y devuelve el precio con el porcentaje descontado.
-}

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

-- Esto hizo llorar a Bob Esponja por 10hs seguidas:
-- descontar 20 (agregar "remolacha" (agregar "zanahoria" (agrandar (agrandar cuartoDeLibra)))) 
-- Esto está mucho mejor:
-- ((descontar 20).(agregar "remolacha").(agregar "zanahoria").agrandar.agrandar) cuartoDeLibra
-- De hecho, muchos de estos paréntesis no son necesarios porque la composición tiene menor precedencia que la aplicación,
-- los únicos que sí hacen falta son estos:
-- (descontar 20.agregar "remolacha".agregar "zanahoria".agrandar.agrandar) cuartoDeLibra
-- Porque queremos aplicar cuartoDeLibra a la función resultante de componer todas esas funciones.

-- Usando COMPOSICIÓN y APLICACIÓN PARCIAL tenemos una forma de trabajar más linda y poderosa
-- Si Juan siempre que pide un combo repite ese mismo proceso de agrandar dos veces, agregarle zanahoria y remolacha
-- y usar un descuento del 20%, podemos abstraer esa idea en una función directamente:
comboJuanizado :: Int -> Int
comboJuanizado = descontar 20.agregar "remolacha".agregar "zanahoria".agrandar.agrandar

-- Esta función está definida usando notación point-free, porque se define en términos de otra función en vez de
-- en términos de la aplicación de funciones. Es equivalente a:
comboJuanizado' combo = (descontar 20.agregar "remolacha".agregar "zanahoria".agrandar.agrandar) combo

-- ¿Cuáles son Funciones en comboJuanizado?:
-- descontar :: Int -> Int -> Int 
-- agregar :: String -> Int -> Int 
-- agrandar :: Int -> Int
-- descontar 20 :: Int -> Int 
-- agregar "remolacha" :: Int -> Int 
-- agregar "zanahoria" :: Int -> Int 
-- descontar 20.agregar "remolacha".agregar "zanahoria".agrandar.agrandar :: Int -> Int 
-- (.) SI! La composición es una función, más adelante volveremos sobre esto 
--     y les diremos de qué tipo es es menos mágico de lo que parece

--Otras versiones de funciones definidas en esta clase usando aplicación parcial y composición:
descontar' porcentaje precio = ((precio -).(`div` 100).(precio  *)) porcentaje

esCuadradoPerfecto' = (==0).decimalPart.sqrt

dispersion' d1 d2 d3 = (max d1.max d2) d3 - (min d1.min d2) d3
