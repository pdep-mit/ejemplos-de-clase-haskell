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

--fantasma 3 (\persona -> edad persona < 13 && tieneItem "disfraz de oveja" persona)

experienciaQuePuedeGanar :: Persona -> [Criatura] -> Int
experienciaQuePuedeGanar persona listaDeCriaturas =
 (`subtract` (nivelDeExperiencia persona)) 
    . nivelDeExperiencia 
    . enfrentamientosSucesivos persona $ listaDeCriaturas

enfrentamientosSucesivos :: Persona -> [Criatura] -> Persona
enfrentamientosSucesivos =
    foldl enfrentarCriatura 