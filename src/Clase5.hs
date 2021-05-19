module Clase5 where

import Text.Show.Functions

-- código inicial

type Barrio = String
type Mail = String
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

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs
  ++ [x] ++
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

mayor''' f valor1 valor2 = (>) (f valor1) (f valor2) -- (>) en forma prefija
mayor'' f valor1 valor2 = (>) (f valor1) . (f) $ valor2 -- (>) aplicada parcialmente + composición
mayor' f valor1 = (>) (f valor1) . (f) -- Lo mismo que arriba con notación point-free


mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f valor1 valor2 = f valor1 > f valor2

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f valor1 valor2 = f valor1 < f valor2




{-
Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.
-}

-- *Clase5> ordenarSegun (menor length) ["hola", "como", "estas"]
-- ["como","hola","estas"]
-- *Clase5> ordenarSegun (mayor length) ["hola", "como", "estas"]
-- ["estas","como","hola"]

-- *Clase5> ordenarSegun (mayor precio) deptosDeEjemplo



type Requisito = Depto -> Bool
{-
Definir las siguientes funciones para que puedan ser usadas como
requisitos de búsqueda:

- ubicadoEn que dada una lista de barrios que le interesan al usuario,
retorne verdadero si el departamento se encuentra en alguno de los barrios
de la lista.
-}

ubicadoEn :: [Barrio] -> Requisito
-- ubicadoEn barrios depto = elem (barrio depto) barrios
ubicadoEn barrios depto = (`elem` barrios) . barrio $ depto
ubicadoEn' barrios depto = ( (`elem` barrios) . barrio ) depto
ubicadoEn'' barrios = (`elem` barrios) . barrio
ubicadoEn''' barrios = flip elem barrios . barrio

{-
- cumpleRango que a partir de una función y dos números, indique si el
valor retornado por la función al ser aplicada con el departamento se
encuentra entre los dos valores indicados.
-}

cumpleRango :: (Depto -> Int) -> Int -> Int -> Requisito
cumpleRango'' f minimo maximo depto = between minimo maximo (f depto)
cumpleRango' f minimo maximo depto = between minimo maximo . f $ depto
cumpleRango f minimo maximo = between minimo maximo . f


-- Definir la función cumpleBusqueda que se cumple si todos los requisitos
-- de una búsqueda se verifican para un departamento dado.
-- type Busqueda = [Requisito]

cumpleBusqueda :: Busqueda -> Depto -> Bool

cumpleBusqueda busqueda depto = all (\requisito -> requisito depto) busqueda
cumpleBusqueda'' busqueda depto = all ($ depto) busqueda
cumpleBusqueda' busqueda depto = all (cumpleRequisito depto) busqueda

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto


{-
Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.
-}

-- ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
buscar :: (Depto -> Depto -> Bool) -> Busqueda -> [Depto] -> [Depto]
buscar criterioDeOrdenamiento busqueda deptos = (ordenarSegun criterioDeOrdenamiento.filter (cumpleBusqueda busqueda)) deptos
buscar' criterioDeOrdenamiento busqueda = ordenarSegun criterioDeOrdenamiento.filter (cumpleBusqueda busqueda)

{-
Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:
- Encontrarse en Recoleta o Palermo
- Ser de 1 o 2 ambientes
- Alquilarse a menos de $6000 por mes
-}
busquedaDeEjemplo :: Busqueda
busquedaDeEjemplo = [cumpleRango ambientes 1 2, ubicadoEn ["Palermo", "Recoleta"], (<6000).precio ]
{-
> buscar (mayor superficie) busquedaDeEjemplo deptosDeEjemplo
[Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]
-}


{-
Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.
-}


{-
data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
} deriving Show
-}

--cumpleBusqueda :: Busqueda -> Depto -> Bool
personasDeEjemplo :: [Persona]
personasDeEjemplo = [Persona "pepito@gmail.com" [busquedaDeEjemplo, [(>70).superficie]], Persona "juanita@gmail.com" []]

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto personas = map mail (filter (estaInteresada depto) personas)

-------------------------- [Persona] -> [Mail] ---- [Persona] -> [Persona]
mailsDePersonasInteresadas' depto = map mail  .  filter (estaInteresada depto)

estaInteresada depto persona = any (flip cumpleBusqueda depto) . busquedas $ persona