
{-
Ejercicio cuantoPagaCadaUno pero con composición y point free

pizzasTotales = techo.(/8).(*3)

pizzasPrecio precio = ((*precio).pizzasTotales) 


CLase 11-4 Pattern Matching, Listas, Tuplas, Recursividad, Data

Pattern Matching - Encaje de Patrones
-}

esFinDeSemana "sabado" = True
esFinDeSemana "domingo" = True
esFinDeSemana "lunes" = False
esFinDeSemana "martes" = False
esFinDeSemana "miercoles" = False
esFinDeSemana "jueves" = False
esFinDeSemana "viernes" = False

esFinDeSemana _ = False 

--El guión bajo es nuestra variable anónima


--implementación con GUARDAS

esFinDeSemana' dia 
 |dia == "sabado" = True
 |dia == "domingo" = True
 |otherwise = False

esFinDeSemana :: String -> Bool
esFinDeSemana' :: String -> Bool

{-
**Nunca poner una sentencia debajo de un patrón que absorve esa sentencia


Recursividad

Es una función que se utiliza a sí misma

Factorial

f(x) 
	|x * f(x-1) si x > 0
	|1 si x = 0


CASO BASE: paso que se puede resolver sólo, sin volver a llamarse a sí mismo
-}

--con guardas
factorial n
  |n > 0 = n * factorial (n-1)
  |n == 0 = 1

--con pattern matching
--Y acá? Ojo, esta función matchea también con números negativos
factorial 0 = 1 ------- el caso base va primero, porque es más específico 
factorial n = n * factorial (n-1)

factorial :: Int -> Int

factorialPrima 0 = 1
factorialPrima n
  |n>0 = n * factorialPrima (n-1)




--Tuplas: T- Uplas, grupos de T

--type -- se usa para especificar el tipo de un dato; LOS TIPOS SIEMPRE VAN CON LA PRIMER LETRA EN MAYÚSCULA 

type Coordenadas3d = (Int, Int, Int)


--Pero, todos tienen que ser del mismo tipo? No necesariamente, las tuplas permiten agrupar datos de distintos tipos

--Limitaciones: los tipos necesitan ser los mismos que los que definiste en el tipo, y también la misma cantidad
--Es decir, si definiste una tupla de tres elementos, esa tupla sólo va a poder tener tres elementos

sumaCoordenadas (x, y, z) = x + y + z
sumaCoordenadas :: Coordenadas3d -> Int
sumaCoordenadas' :: (Int, Int, Int) -> Int
sumaCoordenadas' = sumaCoordenadas --sumaCoordenadas' es la misma funcion que sumaCoordenadas, y vale con cualquiera de los dos tipos

--Pero y como sabe de donde salieron x, y e z? Por pattern matching!  

ultimaCoordenada (_, _, z) = z
ultimaCoordenada :: (a, b, c) -> c

--POKEMON, GOTTA CATCH EM ALL!

-- type Pokemon = (String, Int)

-- nombre = fst -- Por qué? Mejora la expresividad; además es point free!

{-Pero señor Fabricio, qué es point free?

Es cuando escribís una función sin el parámetro


Ahora modelemos una carta

-}

type Carta = (String, Int)

basto :: Int -> Carta

basto num = ("Basto", num)


{-}
Y ahora si hago esta consulta, funciona?
>nombre ("Basto", 10)
"Basto"

Apa, pero la función nombre yo la armé para pokemones, y el 10 de basto no es un pokemon

Bienvenido al mundo de Datas




data Pokemon = UnPokemon {
			nombre :: String,
			nivel :: Int
}


Momento, qué es esto?

Pokemon es nombre de nuestro data
UnPokemon es el constructor de nuestro data
Lo que va entre llaves son los elementos que va a tener nuestro data, y en la definición se separan con comas
Y también son funciones, que nos permiten acceder a ese elemento particular de nuestro data (como fst en tuplas)

Genial, entonces creemos un pokemon
-}

charmander = UnPokemon "Charmander" 5

{-
>nombre charmander
"Charmander"

UnPokemon :: String -> Int -> Pokemon

Y cómo hacer que mi pokemon suba de nivel?

Creamos un nuevo Charmander. Whaaaaaaat? Sí, en funcional no se guardan datos, entonces creamos un nuevo charmander que estructuralmente sea igual 
a nuestro antiguo charmander, pero con un nivel mayor

Entonces, si le doy un caramelo raro, mi pokemon sube de nivel. Escribamos esa función
-}

carameloRaro (UnPokemon nombre nivel) = UnPokemon nombre (nivel + 1)
{-
Y ahora, si pregunto el nivel de Charmander?

>nivel charmander
5

Pero pará, yo le dí un caramelo, por qué no aumentó de nivel?
Porque en funcional, las funciones no tienen efecto. Mi Charmander original siempre va a tener nivel = 5
Esto se llama INMUTABILIDAD, es la idea de que las cosas no cambian

Y si necesito que mi pokemon aumente de nivel? Creo un nuevo pokemon, con un nivel mayor

LISTAS

-> "Equivalente" a un array
-> Siempre van a tener elementos del mismo tipo
 
 Ejemplos felices: ["Hola", "Chau"], []
 Ejemplo no feliz: ["Hola", 2]


Las listas también tienen Patrones

Todas las listas de por lo menos un elemento van a tener una cabeza, que será el primer elemento
Y una cola, que es una lista de todos los elementos restantes de la lista

Entonces si tengo una lista numeros = [1, 2, 3, 4], la cabeza de numeros va a ser 1, y la cola va a ser la lista [2, 3, 4, 5]
Si tengo la lista [1], la cabeza va a ser 1, y la cola va a ser la lista vacía []
-}

f1 [] = True
f2 [x] = True
f3 [x, y] = True
f4 [x:xs] = True
f5 x = True


{-
La función (:) es un constructor de tipo de datos lista

1: [2, 3] me devuelve [1, 2, 3]

[1, 2, 3] : [1, 2] me devuelve una lista de listas, [[1, 2, 3], [1, 2]]


(:) :: a -> [a] -> [a]


Otra función copada para concatenar listas: ++

Ej: [1, 2, 3] ++ [4, 5] nos devuelve [1, 2, 3, 4, 5]


Maiu, Maiu, puedo componer con (:)?

>((: [1, 2, 3]).length) [1...10]
[10, 1, 2, 3]


Qué pasó acá?

[1...10] nos devuelve una lista del 1-10, que se aplica en el length
Length nos devuelve la cantidad de elementos que tiene la lista que le pasamos, que en este caso es 10
Y después volvemos al ejemplo anterior, y quedaría 10 : [1, 2, 3]


Volvamos a los pokemones

Cómo hago para poder mostrar mi pokemon en la consola, y poder compararlo por igualdad?
-}

-- data Pokemon = UnPokemon {
-- 				nombre :: String,
-- 				nivel :: Int
-- } deriving (Eq, Show)



-- Y para ordenarlo?

data Pokemon = UnPokemon {
 nombre :: String,
 nivel :: Int
} deriving (Eq, Show, Ord)

--Haskell hace las comparaciones en el orden en el que definimos los atributos; en este caso compararía por el nombre



--Ejercicio de The Good Place

--Nos interesa modelar las personas

data Persona = UnaPersona {
 nombrePersona :: String,
 edad :: Int,
 acciones :: [Accion]
}

type Accion = (String, Int)
--(descripción y puntaje)
puntos (_, p) = p

--Como sumariamos el puntaje de cada persona? Con recursividad.

puntaje :: [Accion] -> Int

puntaje ((_, puntos): acciones) = puntos + puntaje acciones

--Y no olvidemos el caso base:

puntaje [] = 0



vaAGoodPlace :: Persona -> Bool

vaAGoodPlace persona = edad persona * 10 > puntaje (acciones persona)



--Queremos saber qué acción es noble, y si es noble que se incluya en una lista donde tendré todas mis descripciones

esNoble :: Accion -> Bool

esNoble = (>2000).puntos

descripcion = fst

descripcionesNobles :: Persona -> [String]

descripcionesNobles (UnaPersona nombre edad []) = [] 

descripcionesNobles (UnaPersona nombre edad (accion : acciones))
 |esNoble accion = descripcion accion : (descripcionesNobles (UnaPersona nombre edad acciones))
 |otherwise = (descripcionesNobles (UnaPersona nombre edad acciones))

