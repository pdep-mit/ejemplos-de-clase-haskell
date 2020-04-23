module Clase3 where

type Edad = Int

esMayor :: Edad -> Bool
esMayor = (>= 18)

doble :: Num a => a -> a
doble = (2 *)

alMenosCero :: (Num a, Ord a) => a -> a
alMenosCero = max 0

---------------------------------------------
--- Ejemplo composición
--- Ver PPT de la clase
---------------------------------------------

-- Punto de partida
-- La implementación es irrelevante,
-- sólo lo definimos para analizar el tipado,
-- no esperamos poder evaluarlas
piano :: Midi -> Audio
piano = undefined

bajarTono :: Midi -> Midi
bajarTono = undefined

-- Por poner algo...
type Midi = String
type Audio = String

-------------------------
-- Primera iteración
-------------------------
-- Sin composición
pianoMasGrave :: Midi -> Audio
pianoMasGrave midi = piano (bajarTono midi)

-- Con composición
pianoMasGrave' :: Midi -> Audio
pianoMasGrave' = piano . bajarTono

-------------------------
-- Segunda iteración
-------------------------

ajustarTono :: Int -> Midi -> Midi
ajustarTono = undefined

pianoAjustable :: Int -> Midi -> Audio
pianoAjustable delta midi = piano (ajustarTono delta midi)

-- ESTO NO TIPA!!
-- pianoAjustable' :: Int -> Midi -> Audio
-- pianoAjustable' = piano . bajarTono

-- ESTO SÍ TIPA!
pianoAjustable' :: Int -> Midi -> Audio
pianoAjustable' delta midi = (piano . ajustarTono delta) midi

-- ESTO TAMBIÉN!
pianoAjustable'' :: Int -> Midi -> Audio
pianoAjustable'' delta = piano . ajustarTono delta

---------------------

f1 a b = ((+ a).(*2)) b

f1' a b = (+ a).(*2) $ b

f1'' a = (+ a).(*2)

----------------

g a = (a *).(+ a)

-------------

h :: Ord a => a -> a -> a
h a b = max a b

h' :: Ord a => a -> a -> a
h' a = max a

h'' :: Ord a => a -> a -> a
h'' = max

---------
i :: Num a => a -> a -> a
i x y = (y +).(*7) $ x

noI y = (y +).(*7)

type Carta = (Int, String)
esAs :: Carta -> Bool
esAs = (==1).fst