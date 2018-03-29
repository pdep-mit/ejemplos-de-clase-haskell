-- Primeros ejemplos, se pone en evidencia que no importa el orden de las declaraciones
triple x = 3 * x

sextuple x = doble (triple x)

doble x = 2 * x

----------
-- Ejemplo básico de guardas, importa el orden de las condiciones y el tabulado
valorAbsoluto x
  | x >= 0 = x
  | otherwise = -x

----------------------------
-- Ejercicios más complejos
----------------------------

-- Diferentes soluciones para la función xor
xor b1 b2 = not b1 == b2

-- Esta solución necesita que se explicite el tipo para acotar el dominio
-- De lo contrario el tipo es Eq a => a -> a -> a, permitiendo consultar xor' 3 5
xor' :: Bool -> Bool -> Bool
xor' b1 b2 = b1 /= b2

-- Este no es un buen uso de guardas, evitar su uso para retornar booleanos
xor'' b1 b2
  | b1 && not b2 = True
  | b2 && not b1 = True
  | otherwise = False

-- Esta alternativa es mejor que xor''
xor''' b1 b2 = b1 && not b2 || b2 && not b1

-----------

tipoDeNota :: Int -> String
tipoDeNota nota
  | notaPromocion nota = "Promociona"
  | notaAprobada nota = "Aprobado"
  | esNotaValida nota = "Gracias, vuelva prontos"

notaAprobada nota = esNotaValida nota && nota >= 6
notaPromocion nota = esNotaValida nota && nota >= 8

esNotaValida nota = nota >= 1 && nota <= 10

-----------

-- un año es bisiesto si es multiplo de 4 pero no de 100, o si es multiplo de 400
esBisiesto :: Int -> Bool
esBisiesto anio 
  = esMultiplo anio 4 && 
     not (esMultiplo anio 100) ||
       esMultiplo anio 400

-- Otro ejemplo de mal uso de guardas
esBisiesto' anio
  | esMultiplo anio 4 && 
     not (esMultiplo anio 100) = True
  | esMultiplo anio 400 = True
  | otherwise = False

-- Esta abstracción ayuda a que sean más entendibles ambas soluciones
esMultiplo x y = mod x y == 0