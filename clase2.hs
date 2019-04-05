--Desafíos del inicio de clase:

--Ya se cuenta con la función esMultiploDe
--Definir esBisiesto (si es multiplo de 4 y no 
--de 100 o si es multiplo de 400)

--esBisiestoAire :: Int -> Bool
--esBisiestoAire anio = esMultiploDe 4 anio && not (esMultiploDe 100 anio) || esMultiploDe 400 anio

-- Considerando length :: String -> Int
-- 1) inferir el tipo de f x y z = length y *2 <= x || z
-- f :: Int -> String -> Bool -> Bool

-- GUARDAS

modulo x 
 | x >= 0 = x
 | otherwise = -x

diasEnElMes :: Mes -> Int
diasEnElMes mes
 | mes == Abril || mes == Junio || mes == Septiembre || mes == Noviembre = 30
 | mes == Febrero = 28
 | otherwise = 31

data Mes = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Septiembre | Octubre | Noviembre | Diciembre deriving(Eq)

-- PATTERN MATCHING
diasEnElMes' Febrero = 28
diasEnElMes' Abril = 30
diasEnElMes' Junio = 30
diasEnElMes' Septiembre = 30
diasEnElMes' Noviembre = 30
diasEnElMes' _ = 31

-- Usamos una combinación de Pattern Matching y guardas:

-- diasEnElMesDelAnio :: Mes -> Int -> Int
-- diasEnElMesDelAnio Febrero anio
--  | esBisiestoAire anio = 29

-- diasEnElMesDelAnio mes _ = diasEnElMes' mes

--Desafio! XOR (Pero con PATTERN MATCHING)
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- TUPLAS

--(X, Y, Z) -> Coordenada
--(Int, Int, Int) -> Cada uno representa algo distinto
origen = (0, 0, 0)

--(1, 2, 3) + (5, 6, 8) = ???

--Alias (nombrecito) de tipo
type Coordenada3D = (Int, Int, Int)

sumarCoordenadas :: Coordenada3D -> Coordenada3D -> Coordenada3D
sumarCoordenadas (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

--Obtener el primero de una tupla cualquiera? (1, "Hola") -> 1?

fst' :: (a, b) -> a
fst' (x1, _) = x1
snd' :: (a, b) -> b
snd' (_, x2) = x2

-- DATA

--alumno: (nombre, legajo, promedio) representado por (String, Int, Float)
data Alumno = UnAlumno {
    legajo :: Int,
    promedio :: Float,
    cantMateriasAprobadas :: Int
}deriving(Show, Eq)

--matcheando con pattern matching
mora = UnAlumno 1595933 10.0 20

--utilizando la construcción del data (no depende del orden)
santi = UnAlumno{
    legajo = 1591234,
    cantMateriasAprobadas = 30,
    promedio = 11.0
}

--leVaBien si el promedio es mayor o igual a 8

--Con pattern matching: Un cambio en la estructura puede hacer que no anda más :(
leVaBien (UnAlumno _ promedio _) = promedio >= 8

--Utilizando las funciones que me da el data puedo eliminar ese problema!
leVaBien' alumno = promedio alumno >= 8

--promedio :: Alumno -> Float
--cantMateriasAprobadas :: Alumno -> Int

--aprobarMateria, +1 en la cantidad de materias

--Esto NO funciona -> No se asigna!!
--aprobarMateria alumno = cantMateriasAprobadas alumno + 1 

--INMUTABILIDAD
aprobarMateria :: Alumno -> Alumno

--Genero un nuevo alumno con el constructor y el dato modificado
aprobarMateria (UnAlumno legajo promedio cantMaterias) = 
    UnAlumno legajo promedio (cantMaterias + 1)

-- ¿Qué vimos?
-- Guardas
-- Pattern Matching
-- Typeclasses
-- Tuplas
-- Data
-- Inmutabilidad