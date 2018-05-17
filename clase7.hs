-- Samurai Jack
-- Enunciado: https://docs.google.com/document/d/1mhQ2R8VjpoVrQ5JroYbkiBHjZME9gw6jEn6OI2q6Q2U

data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
  } deriving (Show, Eq)

data Elemento = UnElemento {
  tipo :: String,
  ataque :: (Personaje-> Personaje),
  defensa :: (Personaje-> Personaje)
}

instance Show Elemento where
  show = tipo

instance Eq Elemento where
  (==) elemento1 elemento2 = tipo elemento1 == tipo elemento2

-- Punto 1
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio (UnPersonaje nombre salud elementos _) =
  UnPersonaje nombre salud elementos anio

-- Equivalente con azúcar sintáctico que nos aporta el data definido como lo hacemos normalmente:
-- <DISCLAIMER>
-- sólo usar este azúcar sintáctico en funciones muy chiquititas
-- que sólo hagan un cambio a la vez y puedan ser fácilmente reutilizadas y combinadas entre sí
-- No usar esto por todos lados (de la misma forma que no está bueno
-- usar pattern matching por todos lados)
mandarAlAnio' :: Int -> Personaje -> Personaje
mandarAlAnio' anio personaje = personaje { anioPresente = anio }
-- Esta sintaxis tiene la ventaja de que es menos verbosa y que se ve menos afectada
-- por cambios estructurales, pero no es una función, y en funcional queremos trabajar con funciones!
-- El abuso de esta notación lleva a no explotar los conceptos ricos de funcional que nos interesan
-- que son orden superior, aplicación parcial y composición. Si no saben cuando detenerse, no lo usen.
-- </DISCLAIMER>

meditar :: Personaje -> Personaje
meditar = modificarSalud (*1.5)

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio = modificarSalud (max 0 . flip (-) danio)

modificarSalud :: (Float -> Float) -> Personaje -> Personaje
-- Abstracción útil, super reutilizable :D
-- Acá vale usar el azúcar sintáctico o el equivalente con pattern matching
-- Una solución que no abstrae esta idea, indefectiblemente repite lógica
-- entre meditar y causarDanio con o sin sintaxis cheta
modificarSalud transformacion personaje =
  personaje { salud = (transformacion . salud) personaje }

-- Punto 2
esMalvado :: Personaje -> Bool
esMalvado personaje =
  (any (esDeTipo "Maldad") . elementos) personaje

esDeTipo unTipo elemento = tipo elemento == unTipo

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento =
  salud personaje - salud (ataque elemento personaje)

danioQueProduce' personaje elemento =
  ((salud personaje -) . salud . ataque elemento) personaje

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos =
  filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal personaje enemigo =
  (any (tieneAtaqueMortal personaje) . elementos) enemigo

tieneAtaqueMortal personaje elemento =
  danioQueProduce personaje elemento == salud personaje

tieneAtaqueMortal' personaje elemento =
  (estaMuerto . ataque elemento) personaje

estaMuerto  = ((==0).salud)

-- Punto 3
noHacerNada = id

concentracion :: Int -> Elemento
concentracion nivelDeConcentracion =
  UnElemento { tipo = "Magia",
               ataque = noHacerNada,
               defensa = (!! nivelDeConcentracion) . iterate meditar }
               -- equivalente con composición y aplicación parcial para:
               -- defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion) }
               -- otra versión super interesante:
               -- defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)
               -- por ejemplo (concentracion 3) resultaría en meditar.meditar.meditar

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad unEsbirro

unEsbirro :: Elemento
unEsbirro = UnElemento "Maldad" (causarDanio 1) noHacerNada

jack :: Personaje
jack = UnPersonaje {
  nombre = "Jack",
  salud = 300,
  elementos = [concentracion 3, katanaMagica],
  anioPresente = 200
}

katanaMagica = UnElemento "Magia" (causarDanio 1000) noHacerNada

aku :: Int -> Float -> Personaje
-- Este lo salteamos en clase por falta de tiempo, agrego una solución
-- Como se puede ver aku es recursivo porque tiene portalAlFuturoDesde como elemento
-- cuya defensa es generar un nuevo aku en otro año
aku anio saludInicial = UnPersonaje {
  nombre = "Aku",
  salud = saludInicial,
  anioPresente = anio,
  elementos = concentracion 4 : portalAlFuturoDesde anio : esbirrosMalvados (100 * anio)
}
portalAlFuturoDesde anio = UnElemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo.salud)
  where anioFuturo = anio + 2800

-- Punto 4
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar personaje funcion = funcion personaje
afectar' = flip ($)

-- Punto 5 (inferencia)
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

f :: (Eq t1, Num t2) =>
     (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
