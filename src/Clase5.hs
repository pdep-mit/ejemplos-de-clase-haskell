module Clase5 where

import Text.Show.Functions

-- código inicial

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
} deriving Show

ordenarSegun :: (a -> a -> Bool)-> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo",
  Depto 1 45 3500 "Villa Urquiza",
  Depto 2 50 5000 "Palermo",
  Depto 1 45 5500 "Recoleta"]

-- solución

{-
Definir las funciones mayor y menor que reciban una función y dos valores,
 y retorna true si el resultado de evaluar esa función sobre el primer
 valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.
-}
-- ordenarSegun :: (a -> a -> Bool)-> [a] -> [a]
-- ordenarSegun (menor length) ["hola", "nahue", "y", "mora"]
mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f valor1 valor2 = f valor1 > f valor2
mayor' f valor1 = (f valor1 >).f

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f valor1 valor2 = f valor1 < f valor2

{-
Definir las siguientes funciones para que puedan ser usadas como
requisitos de búsqueda:
- ubicadoEn que dada una lista de barrios que le interesan al usuario,
retorne verdadero si el departamento se encuentra en alguno de los barrios
de la lista.
- cumpleRango que a partir de una función y dos números, indique si el
valor retornado por la función al ser aplicada con el departamento se
encuentra entre los dos valores indicados.
-}

--ubicadoEn :: [Barrio] -> (Depto -> Bool)
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barriosDeInteres departamento = elem (barrio departamento) barriosDeInteres
ubicadoEn' barriosDeInteres departamento = flip elem barriosDeInteres . barrio $ departamento
ubicadoEn'' barriosDeInteres = flip elem barriosDeInteres . barrio

--cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango :: (Num a, Ord a) => (Depto -> a) -> a -> a -> Requisito
cumpleRango f numero1 numero2 = between numero1 numero2 . f

-- Definir la función cumpleBusqueda que se cumple si todos los requisitos
-- de una búsqueda se verifican para un departamento dado.

-- type Requisito = Depto -> Bool
-- type Busqueda = [Requisito]
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto busqueda = all (cumpleRequisito depto) busqueda
cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto

{-
Busqueda que tenga estos requisitos:
- Se encuentren en Recoleta o Palermo
- Sean de 1 a 2 ambientes
- Que se alquilen a menos de $6000 por mes
-}
busquedaDeEjemplo :: Busqueda
busquedaDeEjemplo = [
  ubicadoEn ["Recoleta", "Palermo"],
  cumpleRango ambientes 1 2,
  --cumpleRango precio 0 6000
  (<6000).precio
 ]

 {-
 Definir la función departamentosBuscadosEnOrdenDeInteres que a partir de
  una búsqueda, un criterio de ordenamiento
  y una lista de departamentos retorne todos aquellos que cumplen
  con la búsqueda ordenados en base al criterio recibido.
 -}
-- cumpleBusqueda :: Depto -> Busqueda -> Bool
-- ordenarSegun :: (a -> a -> Bool)-> [a] -> [a]
departamentosBuscadosEnOrdenDeInteres
  :: (Depto -> Depto -> Bool) -> Busqueda -> [Depto] -> [Depto]
departamentosBuscadosEnOrdenDeInteres criterioDeOrdenamiento busqueda deptos
   = ordenarSegun criterioDeOrdenamiento
     . filter (flip cumpleBusqueda busqueda) $ deptos

{-
> departamentosBuscadosEnOrdenDeInteres (mayor superficie) busquedaDeEjemplo deptosDeEjemplo
[Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},
Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]
-}

{-
Definir la función mailsDePersonasInteresadas que a partir de un
departamento y una lista de personas retorne los mails de
las personas que tienen alguna búsqueda que se cumpla para el
departamento dado.
-}
mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto = map mail . filter (estaInteresada depto)

-- cumpleBusqueda :: Depto -> Busqueda -> Bool
estaInteresada :: Depto -> Persona -> Bool
estaInteresada depto = any (cumpleBusqueda depto). busquedas

personaDePrueba = Persona "pepito@algo.com" [
  [cumpleRango ambientes 1 3],
  busquedaDeEjemplo ]
{-
> mailsDePersonasInteresadas (head deptosDeEjemplo )
 [Persona "pepito@algo.com" [[cumpleRango ambientes 1 3],
 busquedaDeEjemplo ], Persona "juan@algo.com" [],
 Persona "maria@algo.com" [busquedaDeEjemplo ]]
["pepito@algo.com"]
-}