-- Los habitantes springfield van a competir en desafios tratando de alcanzar un record.

data Habitante = UnHabitante {
  peso :: Int,
  metrosDeSalto:: Int,
  minutosEnTerminarUnaComida::Int,
  comidas:: [Comida]
} deriving (Show, Eq)

type Comida = String

homero = UnHabitante {
  peso = 150,
  metrosDeSalto = 1,
  minutosEnTerminarUnaComida = 5,
  comidas = ["Rosquilla", "Rosquilla", "Tocino", "HuevosRevueltos"]
}

marge = UnHabitante {
  peso = 65,
  metrosDeSalto = 10,
  minutosEnTerminarUnaComida = 5,
  comidas=[]
}

-- queremos saber:
-- quienes pueden competir en en  el concurso de salto, que nos da los competidores
-- que pueden saltar mas de una cantidad de metros de alto

quienesSaltanAlto _ [] = []
quienesSaltanAlto metrosCondicion (habitante:postulantes)
  | metrosDeSalto habitante > metrosCondicion = habitante : quienesSaltanAlto metrosCondicion postulantes
  | otherwise = quienesSaltanAlto metrosCondicion postulantes

-- que habitantes pueden terminar su almuerzo en menos de tantos minutos

quienesComenRapido _ [] = []
quienesComenRapido tiempoDeCondicion (habitante:comensales)
  | minutosEnTerminarUnaComida habitante < tiempoDeCondicion = habitante : quienesComenRapido tiempoDeCondicion comensales
  | otherwise = quienesComenRapido tiempoDeCondicion comensales

-- vemos que hay repetición de lógica entre ambas funciones y hacemos una función más genérica quienesPuedenCompetir
-- que nos permita reescribir quienesComenRapido y quienesSaltanAlto usandola.
quienesPuedenCompetir :: (Habitante -> Bool) -> [Habitante] -> [Habitante]
quienesPuedenCompetir _ [] = []
quienesPuedenCompetir condicion (habitante:habitantes)
  | condicion habitante = habitante : quienesPuedenCompetir condicion habitantes
  | otherwise = quienesPuedenCompetir condicion habitantes

quienesSaltanAlto' metrosCondicion habitantes = quienesPuedenCompetir (saltaMasDe metrosCondicion) habitantes
saltaMasDe metros = (> metros).metrosDeSalto

quienesComenRapido' tiempoDeCondicion habitantes = quienesPuedenCompetir (comeMasRapidoQue tiempoDeCondicion) habitantes
comeMasRapidoQue tiempoDeCondicion = (< tiempoDeCondicion).minutosEnTerminarUnaComida

-- En sí, quienesPuedenCompetir no es más que la función filter :: (a -> Bool) -> [a] -> [b], sólo que con un tipo acotado a habitantes
-- No hace falta que la programemos, podríamos haber hecho directamente∷
quienesComenRapido'' tiempoDeCondicion habitantes = filter (comeMasRapidoQue tiempoDeCondicion) habitantes
-- Lo que hace que esta función sólo admita habitantes y no cualquier lista es
-- (comeMasRapidoQue tiempoDeCondicion) :: Habitante -> Bool

-- Si quisiéramos saber dada una lista de habitantes qué personas saltan más alto de la velocidad a la que comen:

ejemploConLambda = filter (\persona -> metrosDeSalto persona > minutosEnTerminarUnaComida persona) [homero, marge]

{-
El criterio que armamos no era tan fácil, y no lo podíamos resolver sólo con composición y aplicación parcial,
por eso usamos una expresión lambda que nos permite definir una función anónima al momento de usarla.
Si bien podríamos definir una función saltaMasAltoDeLoQueTardaEnComer, qué tan probable es que nos sea de utilidad en otra ocasión?
Si no nos sirve como abstracción, no hace falta que la definamos aparte y le pongamos un nombre, vale construirla sólo para pasarla
por parámetro y ya
-}

-- Ahora queremos saber cuántas calorías consumió un habitante
calorias :: Comida -> Int
calorias "Rosquilla" = 400
calorias "Tocino" = 300
calorias "HuevosRevueltos" = 100

caloriasTotales :: Habitante -> Int
caloriasTotales = caloriasComida.comidas

caloriasComida :: [Comida] -> Int
caloriasComida [] = 0
caloriasComida (comida : comidas) = calorias comida + caloriasComida comidas

-- Usando las funciones sum :: Num a => [a] -> a y map :: (a -> b) -> [a] -> [b]
caloriasTotales' = sum.map calorias.comidas

-- Otras funciones útiles son all y any
esCiudadObesa :: [Habitante] -> Bool
esCiudadObesa = all obeso
esCiudadSana :: [Habitante] -> Bool
esCiudadSana = not . any obeso
obeso = (>120).peso

{-
Algo interesante de ver a las funciones como valores no es sólo que podemos pasarlas por parámetro,
podemos hacer cualquier cosa que haríamos con cualquier otro valor, por ejemplo, tener una lista de funciones!
-}
ejemploConListaDeFunciones = all (\f -> f 3) [(>10), (<7), odd]

-- El siguiente no tipa, porque (*3) no retorna Bool, por ende no todos los elementos de la lista son del mismo tipo
-- Y si fuera el único elemento o sea [(*3)] tampoco sería válido porque no es del tipo que espera all
-- ejemploConListaDeFuncionesQueNoTipa = all (\f -> f 3) [(>10), (<7), odd, (*3)]

-- Por último, queremos hacer que un habitante participe de un desafío de comida, donde debe comer todas las comidas que se indiquen
-- teniendo la siguiente función para que alguien coma una comida:
comer (UnHabitante peso metrosDeSalto minutosEnTerminarUnaComida comidas) unaComida =
  UnHabitante (peso + calorias unaComida) metrosDeSalto minutosEnTerminarUnaComida (unaComida : comidas)

-- Si lo quisiéramos resolver recursivamente sería algo así
participarEnDesafioDeComida [] unHabitante = unHabitante
participarEnDesafioDeComida (comida : comidas) unHabitante
  = participarEnDesafioDeComida comidas (comer unHabitante comida)

-- Pero vamos a querer aprovechar funciones de orden superior todo lo que podamos de ahora en más.
-- Para resolver este problema podemos usar la función foldl :: (a -> b -> a) -> a -> [b] -> a
participarEnDesafioDeComida' listaDeComidas unHabitante = foldl comer unHabitante listaDeComidas
