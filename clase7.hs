import Text.Show.Functions

data Persona = UnaPersona {
    nivelDeExperiencia :: Int,
    edad :: Int,
    inventario :: [String]
} deriving (Show, Eq)

data Criatura = UnaCriatura {
    nombre :: String,
    peligrosidad :: Int,
    puedeDeshacerseDeElla :: (Persona -> Bool)
} deriving (Show)

siempreDetras :: Criatura
siempreDetras =
     UnaCriatura { nombre = "siempredetras",
      peligrosidad = 0,
      puedeDeshacerseDeElla = (\_ -> False) }

gnomos :: Int -> Criatura
gnomos cantidad =
     UnaCriatura { nombre = "gnomos",
      peligrosidad = 2 ^ cantidad,
      puedeDeshacerseDeElla = tieneItem "soplador de hojas" }

fantasma :: Int -> (Persona -> Bool) -> Criatura
fantasma categoria asuntoPendiente =
     UnaCriatura { nombre = "fantasma",
      peligrosidad = categoria * 20,
      puedeDeshacerseDeElla = asuntoPendiente }

enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura
    |gano persona criatura =
        aumentarExperiencia (peligrosidad criatura) persona
    |otherwise = escaparse persona

gano :: Persona -> Criatura -> Bool
gano persona criatura =
     puedeDeshacerseDeElla criatura persona

tieneItem :: String -> Persona -> Bool
tieneItem item persona =
    elem item (inventario persona)

escaparse :: Persona -> Persona
escaparse persona = aumentarExperiencia 1 persona

aumentarExperiencia :: Int -> Persona -> Persona
aumentarExperiencia cantidad persona =
    persona { nivelDeExperiencia = (nivelDeExperiencia persona) + cantidad}

experienciaQuePuedeGanar :: Persona -> [Criatura] -> Int
experienciaQuePuedeGanar persona listaDeCriaturas =
 (subtract (nivelDeExperiencia persona)) 
    . nivelDeExperiencia
    . enfrentamientosSucesivos persona $ listaDeCriaturas

enfrentamientosSucesivos :: Persona -> [Criatura] -> Persona
enfrentamientosSucesivos = foldl enfrentarCriatura

{-
Mostrar un ejemplo de consulta para el punto anterior incluyendo las siguientes criaturas:
al siempredetras, a un grupo de 10 gnomos, un fantasma categoría 3 que requiere
que la persona tenga menos de 13 años y un disfraz de oveja entre sus ítems
para que se vaya y un fantasma categoría 1 que requiere que la persona tenga más de 10 de experiencia.
-}

{-
experienciaQuePuedeGanar (UnaPersona 20 12 ["soplador de hojas", "disfraz de oveja", "diario"])
  [siempreDetras, gnomos 10, fantasma 3 (\persona -> edad persona < 13 && tieneItem "disfraz de oveja" persona),
  fantasma 1 ((>10).nivelDeExperiencia)]
-}
