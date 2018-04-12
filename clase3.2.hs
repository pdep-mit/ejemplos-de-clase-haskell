-- Bienvenido al mundo de Datas

data Pokemon = UnPokemon {
  nombre :: String,
  nivel :: Int
} deriving (Show, Eq)


{-
Momento, qué es esto?

- Pokemon es nombre de nuestro tipo de dato.
- UnPokemon es el constructor de nuestro data. Los costructores son, oh sorpresa, funciones! Además se comportarse como todas las funciones que ya vimos, nos permiten usar pattern matching.
- Lo que va entre llaves son los elementos que va a tener nuestro data, y en la definición se separan con comas.
- deriving (Show, Eq) lo agregamos más tarde, para poder mostrar mi pokemon en la consola, y poder compararlo por igualdad con otro pokemon en base a cómo se imprimen y cómo se comparan por igualdad sus componentes.
- También vimos que si le agregamos Ord también se pueden comparar por >, >=, < y <=, qué loco, no? Haskell haría las comparaciones en el orden en el que definimos los componentes; en este caso compararía por el nombre primero, y de ser igual compararía por el nivel.

Esta definición además nos genera funciones llamadas nombre y nivel que nos permiten acceder a ese elemento particular de nuestro data (análogo al fst y snd en tuplas de dos elementos)

Genial, entonces creemos un pokemon
-}

charmander = UnPokemon "Charmander" 5

{-
Hacemos unas pruebitas rápidas:

> nombre charmander
"Charmander"

> :t UnPokemon
UnPokemon :: String -> Int -> Pokemon

------------

Y si queremos hacer que mi Charmander suba de nivel? Parece un problema que requiere de poder producir efecto, cosa que no podemos...

Creamos un nuevo Charmander. Whaaaaaaat? Sí, en funcional no se guardan datos, entonces creamos un nuevo Charmander que estructuralmente sea igual 
a nuestro antiguo Charmander, pero con un nivel mayor

Entonces, si le doy un caramelo raro, "mi pokemon" sube de nivel. Escribamos esa función, que de paso nos sirve para ver cómo usar pattern matching con pokemones.
-}

carameloRaro :: Pokemon -> Pokemon
carameloRaro (UnPokemon nombre nivel) = UnPokemon nombre (nivel + 1)

{-
> carameloRaro charmander
UnPokemon {nombre = "Charmander", nivel = 6}

Y ahora, si pregunto el nivel de Charmander?

> nivel charmander
5

Pero pará, yo le dí un caramelo, por qué no aumentó de nivel?
Porque en funcional, las funciones no tienen efecto.
Mi Charmander original siempre va a tener nivel = 5, todos los datos son inmutables, o sea que no cambian.

Y cómo hago para seguir trabajando si mi charmander nunca subió de nivel realmente? 
Sólo hay que usar el resultado con otras funciones, esa es la forma funcionalosa de trabajar, por ejemplo

> (nivel.carameloRaro.carameloRaro) charmander
7

-}

----------------------

{-
Vamos a incorporar el último tipo de dato que nos falta conocer de Haskell: las LISTAS

-> Sirven para modelar conjuntos (como cuando usaban arrays o listas enlazadas en estructurado)
-> Siempre van a tener elementos del mismo tipo
 
 Ejemplos felices: ["Hola", "Chau"], []
 Ejemplo no feliz: ["Hola", 2]  <--- NO TIPA


Las listas también tienen Patrones

Todas las listas de por lo menos un elemento van a tener una cabeza, que será el primer elemento
Y una cola, que es una lista de todos los elementos restantes de la lista  <---- Las listas son estructuras recursivas!!!

Entonces si tengo una lista numeros = [1, 2, 3, 4], la cabeza de numeros va a ser 1, y la cola va a ser la lista [2, 3, 4, 5]
Si tengo la lista [1], la cabeza va a ser 1, y la cola va a ser la lista vacía []

Veamos un poco qué ejemplos patrones podemos usar con listas y probemos cada caso con la lista [] y la lista [1,2,3] para ver cuáles matchean y cuáles no.
-}

f1 [] = True  -- [] matchea, [1,2,3] no
f2 [x] = True  -- no matchea ninguna de las dos, porque requiere exactamente un elemento
f3 [x, y] = True -- no matchea ninguna de las dos, porque requiere exactamente dos elemento
f4 (x:xs) = True -- [] no matchea, [1,2,3] sí, siendo x = 1 y xs = [2,3]
f5 x = True -- ambas matchean

{-
La función (:) es un constructor del tipo de dato lista

1: [2, 3] me devuelve [1, 2, 3], que es lo mismo que escribir 1:2:3:[]

Pregunta: se puede usar una lista como primer parámetro de (:)?
Respuesta: sí, pero teniendo en cuenta que ese sería el tipo de los ELEMENTOS, o sea, sería una lista de listas, por ejemplo:
[1, 2, 3] : [[1, 2]] me devuelve una lista de listas, [[1, 2, 3], [1, 2]]

Veamos el tipo de la función:
(:) :: a -> [a] -> [a]


Otra función copada es el (++), para concatenar listas:

Ej: [1, 2, 3] ++ [4, 5] nos devuelve [1, 2, 3, 4, 5]


Pregunta: Maiu, Maiu, puedo componer con (:)?
Respuesta: es una función más, con la particularidad de ser infija, con lo cual podemos usarla con composición y aplicación parcial

>((: [1, 2, 3]).length) [1...10]
[10, 1, 2, 3]

Qué pasó acá?

- [1...10] nos devuelve una lista del 1-10, que se aplica en el length
- length nos devuelve la cantidad de elementos que tiene la lista que le pasamos, que en este caso es 10
- Y después volvemos al ejemplo anterior, y quedaría 10 : [1, 2, 3]

Entendamos los tipos de las cosas que usamos en este ejemplo:

> :t (: [1, 2, 3])
(: [1, 2, 3]) :: Num a => a -> [a]

> :t length
length :: Foldable t => t a -> Int  (pueden leer eso como [a] -> Int pensando que Foldable t = lista)

Con lo cual podemos ver a ((: [1, 2, 3]).length) como una función de tipo: [a] -> [Int]

Hay muchas cosas locas que se pueden probar, pero mejor bajarlo a un ejercicio concreto
-}

-------------------------------------------
-- Ejercicio integrador de The Good Place
-------------------------------------------

{-
“The Good Place”(TGP) es un increíble vecindario diseñado por un arquitecto de otra dimensión, Michael. Las personas que viven allí fueron seleccionadas por su impecable comportamiento.
¿Con qué criterio? Todas las acciones durante la vida de un humano son puntuadas, las buenas positivamente, y las malas negativamente. Al llegar la hora de la verdad, si la suma de los puntajes de las acciones de una persona supera a su edad multiplicada por 10, es digna de vivir en TGP.
Cuando una persona llega a TGP es recibida personalmente por Michael quien le explica su situación quien le organiza una fiesta de bienvenida. 

Para la fiesta de bienvenida de cada persona, Michael quiere saber si puede honrarla con una hermosa banda que dice “gran persona”, esto ocurre si sus tres últimas acciones fueron buenas. También desea saber las acciones nobles del homenajeado para su discurso. Una acción es noble si tiene más de 800 puntos.
Conocer la mejor acción, o sea la de más alto puntaje, le va a venir de maravilla para el homenaje, para poder cerrar el discurso de bienvenida.
-}

-- Nos interesa modelar las personas

data Persona = UnaPersona {
 nombrePersona :: String,
 edad :: Int,
 acciones :: [Accion]
}

type Accion = (String, Int)
descripcion = fst
puntos = snd

-- Arranquemos por saber si una persona va a TGP
vaAGoodPlace :: Persona -> Bool
vaAGoodPlace persona = edad persona * 10 > puntaje (acciones persona)

-- Como sumariamos el puntaje de una lista de acciones? Podemos hacerlo con recursividad.

puntaje :: [Accion] -> Int

puntaje ((_, puntos): acciones) = puntos + puntaje acciones
--Y no olvidemos el caso base:
puntaje [] = 0


-- Queremos saber qué acción es noble, y si es noble que se incluya en una lista donde tendré todas mis descripciones

esNoble :: Accion -> Bool

esNoble = (>2000).puntos

descripcionesNobles :: Persona -> [String]

descripcionesNobles (UnaPersona nombre edad []) = [] 

descripcionesNobles (UnaPersona nombre edad (accion : acciones))
 |esNoble accion = descripcion accion : (descripcionesNobles (UnaPersona nombre edad acciones))
 |otherwise = (descripcionesNobles (UnaPersona nombre edad acciones))

-- Para practicar: definir las siguientes funciones
-- fueUnaGranPersona :: Persona -> Bool
-- mejorAccion :: Persona -> Accion