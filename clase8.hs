-- numeros primos

esDivisorDe :: Int -> Int -> Bool
esDivisorDe multiplo divisor = multiplo `mod` divisor == 0

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = all (not . esDivisorDe n) [2..raizEntera n]
    where raizEntera = ceiling . sqrt . fromIntegral

primos :: [Int]
primos = filter esPrimo [1..]

todosLosEnterosDesde :: Int -> [Int]
todosLosEnterosDesde n = n : todosLosEnterosDesde (n+1)

todosLosNaturales :: [Int]
todosLosNaturales = todosLosEnterosDesde 1

infinitosUnos :: [Int]
infinitosUnos = 1 : infinitosUnos

-- typeclasses

instance Show (a->b) where
    show _ = "Una Función"

data Fraccion = Fraccion { numerador :: Int, denominador :: Int }

instance Show Fraccion where
    show fraccion | entera fraccion = show (numerador fraccion)
                  | otherwise = show (numerador fraccion) ++ "/" ++ show (denominador fraccion)

entera :: Fraccion -> Bool
entera fraccion = denominador fraccion == 1

instance Eq Fraccion where
    (==) unaFraccion otraFraccion = toFloat unaFraccion == toFloat otraFraccion

instance Ord Fraccion where
    compare unaFraccion otraFraccion = compare (toFloat unaFraccion) (toFloat otraFraccion)

toFloat :: Fraccion -> Float
toFloat n = fromIntegral (numerador n) / fromIntegral (denominador n)

(//) :: Int -> Int -> Fraccion
(//) _ 0 = error "División por 0"
(//) numerador denominador = simplificar (Fraccion numerador denominador)

simplificar :: Fraccion -> Fraccion
simplificar fraccion = Fraccion (numerador fraccion `div` denominadorComun)
                                (denominador fraccion `div` denominadorComun)
    where denominadorComun = gcd (numerador fraccion) (denominador fraccion)

instance Num Fraccion where
    (+) unaFraccion otraFraccion = (numerador unaFraccion * denominador otraFraccion + numerador unaFraccion * denominador otraFraccion) //
                                   (denominador unaFraccion * denominador otraFraccion)

    negate fraccion = negate (numerador fraccion) // denominador fraccion

    (*) unaFraccion otraFraccion = (numerador unaFraccion * numerador otraFraccion) //
                                   (denominador unaFraccion * denominador otraFraccion)

    abs fraccion | fraccion < 0 = negate fraccion
                 | otherwise = fraccion

    fromInteger integer = fromInteger integer // 1

    signum 0 = 0
    signum fraccion | fraccion > 0 = 1
                    | fraccion < 0 = -1
