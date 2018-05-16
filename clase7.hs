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
mandarAlAnio = undefined
meditar = undefined
causarDanio = undefined

-- Punto 2
esMalvado = undefined

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce = undefined

enemigosMortales = undefined

-- Punto 3
concentracion = undefined

esbirrosMalvados = undefined

jack = undefined

aku :: Int -> Float -> Personaje
aku = undefined

-- Punto 4
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar = undefined

-- Punto 5 (inferencia)
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))
