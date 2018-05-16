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

mandarAlAnio' anio personaje = 
  personaje { anioPresente = anio }  

meditar :: Personaje -> Personaje
meditar = modificarSalud (*1.5)

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio = modificarSalud (max 0 . flip (-) danio)

modificarSalud :: (Float -> Float) -> Personaje -> Personaje 
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
concentracion :: Int -> Elemento
concentracion nivelDeConcentracion =
  UnElemento { tipo = "Magia",
               ataque = id,
               defensa = (!! nivelDeConcentracion) . iterate meditar }
               --defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion) }
               --defensa = foldr1 (.) (replicate nivelDeConcentracion meditar) }
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad unEsbirro
unEsbirro :: Elemento
unEsbirro = UnElemento "Maldad" (causarDanio 1) noHacerNada

noHacerNada = id

jack :: Personaje
jack = UnPersonaje {
  nombre = "Jack",
  salud = 300,
  elementos = [concentracion 3, katanaMagica],
  anioPresente = 200
}

katanaMagica = UnElemento "Magia" (causarDanio 1000) noHacerNada

aku :: Int -> Float -> Personaje
aku = undefined

-- Punto 4
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)


usarElementos :: (Elemento -> Personaje -> Personaje) -> 
  Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje
(map funcion elmentos)

afectar personaje funcion = funcion personaje

-- Punto 5 (inferencia)
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))
