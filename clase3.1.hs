{-
Ejercicio resuelto cuantoPagaCadaUno, con composición y aplicación parcial.
Importante abstraer!
precioPizzas y pizzasTotales las definimos point free, a modo de práctica
-}

cuantoPagaCadaUno precio comensales = ((/comensales).precioPizzas precio) comensales
precioPizzas precio = ((*precio).pizzasTotales) 
pizzasTotales = techo.(/8).(*3)

-- esta función ya venía dada para que no se tengan que chocar con ese problema
techo = fromIntegral.ceiling

-------------------------
-- Clase del 11/04/18
-------------------------

-- Pattern Matching - Encaje de Patrones

-- Queremos definir una función esFinDeSemana que dado un día de la semana, se cumple sólo si es sabado o domingo
esFinDeSemana :: String -> Bool

esFinDeSemana "sabado" = True
esFinDeSemana "domingo" = True
esFinDeSemana _ = False 

-- El guión bajo es nuestra variable anónima, matchea con cualquier otro valor. Es importante que sea la última definición, dado que es absovente.

-- Esa solución es equivalente a la siguiente implementación con GUARDAS

esFinDeSemana' dia 
 |dia == "sabado" = True
 |dia == "domingo" = True
 |otherwise = False

-- La solución con pattern matching es menos verbosa. Si la condición es sólo comparar el parámetro por igualdad con un valor conocido, podemos usar pattern matching.

--------------------------

{-
Recursividad

Una función es recursiva si se utiliza a sí misma. Ejemplo:

Factorial

f(x) 
	|x * f(x-1) si x > 0
	|1 si x = 0

CASO RECURSIVO: paso que se resuelve volviendo a invocar a la función definida.
CASO BASE: paso que se puede resolver solo, sin volver a llamarse a sí mismo, usado para cortar la recursividad.

Proponemos algunas soluciones a este problema en Haskell:
-}

--con guardas
factorial n
  |n > 0 = n * factorial (n-1)
  |n == 0 = 1
factorial :: Int -> Int  
-- Nos damos cuenta que es necesario acotar el tipo a enteros. Si no lo hacemos va a fallar por error de pattern matching ya que ninguna guarda satisface la condición para números entre -1 y 0, a los cuales se llegaría usando números no enteros. Sería mejor que ni intente resolver el factorial para un tipo inválido.

--con pattern matching
factorial' :: Int -> Int
factorial' 0 = 1 ------- el caso base va primero, porque es más específico 
factorial' n = n * factorial (n-1)
--Nos damos cuenta que esta función matchea también con números negativos, con lo cual si consultamos factorial' (-3) resulta en loop infinito.

--híbrido
--es válido resolver parte del problema con una guarda y parte sólo con pattern matching. Esta solución acota el dominio de forma correcta y aprovecha al máximo las herramientas que tenemos.
factorial'' :: Int -> Int
factorial'' 0 = 1
factorial'' n
  |n>0 = n * factorial'' (n-1)

--------------------------

--Tuplas: T-Uplas, grupos de T elementos
-- Queremos representar la idea de un punto en el espacio 3D. Estaría bueno que podamos agrupar esos tres valores en un único valor. Para eso podríamos usar una tupla de tipo (Int, Int, Int)
 
type Coordenadas3d = (Int, Int, Int)
-- type -- se usa para generar un alias para un tipo de un dato, aporta expresividad, para poder usar ese alias al declarar los tipos de las funciones.
-- LOS TIPOS SIEMPRE VAN CON LA PRIMER LETRA EN MAYÚSCULA

sumaCoordenadas :: Coordenadas3d -> Int
-- Equivalente a sumaCoordenadas :: (Int, Int, Int) -> Int
sumaCoordenadas (x, y, z) = x + y + z

--Pero y como sabe de donde salieron x, y e z? Por pattern matching!  

ultimaCoordenada (_, _, z) = z
-- El tipo que es capaz de inferir Haskell es: ultimaCoordenada :: (a, b, c) -> c
-- Si le explicitamos ultimaCoordenada :: Coordenadas3d -> Int eso lo va a restringir un poco más.

--Todos los elementos de la tupla tienen que ser del mismo tipo? No necesariamente, las tuplas permiten agrupar datos de distintos tipos
--El tipo de los elementos y la cantidad de elementos determina el tipo de la tupla. Veamos otro ejemplo:

{-POKEMON, GOTTA CATCH EM ALL!

Podríamos modelar un pokemon de la siguiente forma, siendo el String su nombre, y el Int su nivel.
-}

type Pokemon = (String, Int)
nombre :: Pokemon -> String
nombre = fst 

-- Supongamos que para el mismo programan queremos modelar una carta
type Carta = (String, Int)

basto :: Int -> Carta
basto num = ("Basto", num)


{-
Y ahora si hago esta consulta, funciona?
>nombre ("Basto", 10)
"Basto"

Apa, pero la función nombre yo la armé para pokemones, y el 10 de basto no es un pokemon, eso debería ser un error de tipos! Pero no lo es, prque type sólo me define un alias, por ende a efectos prácticos los pokemones y las cartas son del mismo tipo: (String, Int).

Para resolver este problema necesitamos que Pokemon no sea sólo (String, Int), sino un tipo nuestro distinto a todo lo demás, que sólo sirva para representar pokemones.

Este apunte continúa en clase3.2.hs
-}