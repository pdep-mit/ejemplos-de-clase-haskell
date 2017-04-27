-- Punto 1
type Dinero = Int
data Persona = Persona {
    nombre :: String,
    dinero :: Dinero,
    suerte :: Int,
    factores :: [Factor]
} deriving Show

-- Arrancamos con una tupla (String, Int), luego agregamos el tercer valor
-- type Factor = (String, Int, Bool)
-- nombreDelFactor (nombre, _, _)= nombre
-- valor (_, valor, _)= valor
-- permitido (_,_, permitido) = permitido

-- Las tuplas pueden cambiarse fácilmente por data ya que estamos, no afecta a las funciones implementadas
data Factor = Factor {
    nombreDelFactor :: String,
    valor :: Int,
    permitido :: Bool
} deriving Show

cosmeFulanito = Persona "Cosme Fulanito" 100 30 
    [Factor "amuleto" 3 True, Factor "manos magicas" 100 False, Factor "paciencia" (-10) True]
elCoco = Persona "El Coco" 20 70
    [Factor "inteligencia" 55 True, Factor "paciencia" 50 True]
    
-- Punto 2
cuantoPara :: String -> (Persona -> Int)
cuantoPara nombreFactor persona  
    | tieneFactor nombreFactor persona 
        = valor (buscarFactor nombreFactor persona)
    | otherwise = 0

tieneFactor nombreFactorBuscado =
    any ((== nombreFactorBuscado).nombreDelFactor).factores
    
-- Tener en cuenta al usar buscarFactor que si no hay ninguno que se llame de esa forma, tira un error (por el head de la lista vacía).
-- En cuantoPara nos aseguramos de que eso no pase validando previamente que lo tenga.
buscarFactor :: String -> Persona -> Factor
buscarFactor nombreFactorBuscado =
    head.filter ((== nombreFactorBuscado).nombreDelFactor).factores 

potencial :: Persona -> Int
potencial persona = suerte persona + div ((sum.map valor.factores) persona) 100
    
tramposa :: Persona -> Bool
tramposa = any (not.permitido).factores
    
-- Punto 3
type FuncionGanancia = Dinero -> Dinero
type CriterioParaGanar = Persona -> Bool
data Juego = Juego {
        nombreJuego :: String,
        cuantoDaDeGanancia :: FuncionGanancia,
        criteriosParaGanar :: [CriterioParaGanar]
    }

ruleta :: Juego
ruleta = Juego "Ruleta"  (*37)  
    [(> 70).potencial , not.tramposa]

maquinita :: Dinero -> Juego
maquinita jackpot = Juego "Maquinita" (jackpot +)
    [(>50).suerte, (>40).cuantoPara "paciencia"]

puedeGanar :: Persona -> Juego -> Bool
puedeGanar persona  =
  all ($ persona). criteriosParaGanar
  
-- Ejemplos de uso:
-- puedeGanar cosmeFulanito ruleta
-- puedeGanar cosmeFulanito (maquinita 1000)

-- solución básica
blackjack :: Juego
blackjack' = Juego "Blackjack" (*2)
    [(\persona -> suerte persona > 30 || tramposa persona)]

-- solución super copada con el oBien de orden superior, reutilizable para otro CriterioParaGanar!!!
blackjack = Juego "Blackjack" (*2)
    [((>30).suerte) `oBien` tramposa]
    
oBien :: CriterioParaGanar -> CriterioParaGanar -> CriterioParaGanar
oBien criterio1 criterio2 persona =
    criterio1 persona || criterio2 persona

-- Punto 4
-- Para las funciones que tienen como objetivo modificar a la persona (que se resuelven retornando una persona nueva)
-- conviene que la persona sea el último parámetro, lo que permite que se puedan combinar fácil entre ellas
-- como pasa con las funciones de listas como map, filter, take...

apostar :: Juego -> Dinero -> Persona -> Persona
apostar juego apuesta persona =
    (jugar juego apuesta.bajarSaldo apuesta) persona

-- Si la persona no tiene suficiente saldo, bajarSaldo tira error, de modo que apostar también lo hará y la persona no va a jugar, que es lo esperado.
-- Si bajarSaldo sólo retornara a la persona igual a la que entró, al usarse en apostar lo mandaríamos a jugar igual, que es incorrecto,
-- a menos que validemos previamente que tenga suficiente saldo. Luego habría que pensar qué debería retornar apostar ante ese escenario...

bajarSaldo :: Dinero -> Persona -> Persona
bajarSaldo dineroAGastar persona
    | dinero persona >= dineroAGastar = sumarSaldo (-dineroAGastar) persona
    | otherwise = error "Saldo insuficiente"

sumarSaldo dinero (Persona nombre dineroActual suerte factores) = Persona nombre (dineroActual + dinero) suerte factores

jugar :: Juego -> Dinero -> Persona -> Persona
jugar juego apuesta persona
    | puedeGanar persona juego 
        = sumarSaldo (cuantoDaDeGanancia juego apuesta) persona
    | otherwise = persona

-- Punto 5
cuantoPuedeConseguir :: Persona -> Dinero -> [Juego] -> Dinero
cuantoPuedeConseguir persona dinero juegos =
    (foldl (flip cuantoDaDeGanancia) dinero.filter (puedeGanar persona)) juegos

cuantoPuedeConseguir' persona dinero juegos =
    (foldr cuantoDaDeGanancia dinero.filter (puedeGanar persona)) juegos

-- Explicación foldl vs foldr --
-- Muchas veces no importa si usamos foldl o foldr y podemos sólo elegir cuál usar 
-- en base al tipo de la función que resulte más conveniente.
-- En esta caso las soluciones se comportan distinto:

-- Veamos como se comporta el foldl
-- *Main> cuantoPuedeConseguir elCoco 10 [blackjack, maquinita 1000, ruleta]
-- 37740 porque hace (((10 * 2) + 1000) * 37)
-- *Main> cuantoPuedeConseguir elCoco 10 [ruleta, maquinita 1000, blackjack]
-- 2740 porque hace (((10 * 37) + 1000) * 2)

-- La versión con foldr responde al revés porque trabaja distinto con la lista
-- *Main> cuantoPuedeConseguir' elCoco 10 [blackjack, maquinita 1000, ruleta]
-- 2740 porque hace (2 * (1000 + (37 * 10)))
-- *Main> cuantoPuedeConseguir' elCoco 10 [ruleta, maquinita 1000, blackjack]
-- 37740 porque hace (37 * (1000 + (2 * 10)))

-- Es por ese motivo que el foldl suele ser más intuitivo, por el orden de lectura.
    