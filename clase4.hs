data Persona = Persona {
 salud :: Int,
 suerte :: Int,
 envenenada :: Bool
} deriving (Show, Eq)

{-
Saber si un ingrediente es exótico,
que retorna True si tiene al menos 20 letras.
-}

-- sin composición
esExotico :: String -> Bool
esExotico ingrediente = length ingrediente > 20

-- con composición (y point free)
esExotico' :: String -> Bool
esExotico' = (>20).length

conEnvenenada :: Bool -> Persona -> Persona
conEnvenenada estadoNuevo unaPersona = Persona{
    salud = salud unaPersona,
    suerte = suerte unaPersona,
    envenenada = estadoNuevo
}

conEnvenenada' :: Bool -> Persona -> Persona
conEnvenenada' estadoNuevo unaPersona =unaPersona{
    envenenada = estadoNuevo
}

--conEnvenenada' es una implementación alternativa a conEnvenada que utiliza un sintax sugar.
-- esto solo es un chiche de haskell que es práctico para hacer estas minifunciones que generan un dato nuevo a partir de otro con un delta
-- es importante limitar el uso de este sintax sugar solo x comodidad, esta herramienta no es una funcion
-- por lo que no nos permite aprovechar el poder que nos dan las funciones en haskell (como por ejemplo componer)
-- es SUPER IMPORTANTE entender que a pesar de la sintaxis bonita esto NO MODIFICA el dato.
-- solo genera uno nuevo a partir de otro cambiando uno de los atributos y manteniendo los demas

conSalud :: Int -> Persona -> Persona
conSalud saludNueva unaPersona = Persona{
    salud = saludNueva,
    suerte = suerte unaPersona,
    envenenada = envenenada unaPersona
}

conSuerte :: Int -> Persona -> Persona
conSuerte suerteNueva unaPersona = Persona{
    salud = salud unaPersona,
    suerte = suerteNueva,
    envenenada = envenenada unaPersona
}

aumentarSalud :: Int -> Persona -> Persona
aumentarSalud unaSalud unaPersona =
    conSalud (unaSalud + salud unaPersona) unaPersona

aumentarSuerte :: Int -> Persona -> Persona
aumentarSuerte unaSuerte unaPersona =
    conSuerte (unaSuerte + suerte unaPersona) unaPersona

{-
Definir la función consumirIngrediente :: String ->
    Persona -> Persona de modo que:
-}

consumirIngrediente :: String -> Persona -> Persona

{-Si el ingrediente es “Bezoar” y la persona está
envenenada, deja de estar envenenada, en caso de
 que no esté envenenada no le produce ningún
 efecto.-}

consumirIngrediente "Bezoar" unaPersona  =
   conEnvenenada False unaPersona

{-Si el ingrediente es “Díctamo”, aumenta en 20
la salud de la persona.-}
consumirIngrediente "Dictamo" unaPersona  =
   aumentarSalud 20 unaPersona
{-
Si el ingrediente es “Valeriana”,
relaja a la persona aumentando en 5 su salud.
-}
consumirIngrediente "Valeriana" unaPersona=
   aumentarSalud 5 unaPersona
{-Si el ingrediente es “Cuerno de unicornio”,
le aumenta la suerte a la persona en 10 y
también le aumenta la salud en 20.
-}
consumirIngrediente "Cuerno de unicornio" unaPersona=
   (aumentarSalud 20.aumentarSuerte 10) unaPersona
-- Consumir el cuerno de unicornio esta utilizando 2 funciones aplicadas parcialmente
-- (aumentarSalud 20) (Persona -> Persona) es una aplicación parcial de aumentarSalud (Int -> Persona -> Persona)
-- Aplicar parcialmente una funcion nos permite generar toda una familia de funciones a partir de ella para representar nuevas ideas
-- De esta forma mientras aumentarSalud es una funcion que recibe un numero N y una Persona P
-- y genera una persona nueva igual a P con su salud aumentada en N
-- (aumentarSalud 20) es una nueva función que recibe una Persona P y retorna una nueva persona igual a P pero con 20 más de salud
-- notese que el orden de los parametros en aumentarSalud, aumentarSuerte y todas estas funciones que representan
-- transformaciones sobre Personas no es random, dejar a la persona como último parametro resulta
-- muy util para componer estas funciones! (o enrealidad los resultados de aplicarlas parcialmente xD)

{-
Para los demás, si es exótico envenena a la persona,
de lo contrario sólo le aumenta la salud en 1.
-}
consumirIngrediente ingrediente unaPersona
   | esExotico ingrediente = conEnvenenada True unaPersona
   | otherwise = aumentarSalud 1 unaPersona


-- En Haskell podemos decir que una funcion es igual a otra
-- de esa forma el tipo de la primera va a ser igual que el tipo de la segunda
-- la funcion consumirIngrediente' es un ejemplo de esto, en vez de decir que
-- consumir un Bezoar recibe una persona y se la aplica a (conEnvenenada False)
-- podemos decir que consumir un Bezoar es igual a (conEnvenenada False)
consumirIngrediente' "Bezoar" = conEnvenenada False
consumirIngrediente' "Dictamo" = aumentarSalud 20
consumirIngrediente' "Valeriana" = aumentarSalud 5
consumirIngrediente' "Cuerno de unicornio" = aumentarSalud 20.aumentarSuerte 10
-- Esta idea de definir funciones en base a otras funciones sin pensar en la aplicación final
-- nos permite poder sacar algunos parametros como pasa en este caso con la persona
-- a esta notación se la suele llamar point free.
-- Lo importante es entender que podemos definir una funcion en base a otras sin necesidad de aplicarlas totalmente.

-- El caso generico de consumir ingrediente también se podria expresar point free de esta manera:
consumirIngrediente' ingrediente
   | esExotico ingrediente = conEnvenenada True
   | otherwise = aumentarSalud 1
-- Esto es posible gracias a que la persona sólo se usaba para aplicar como último parámetro a las funciones usadas
-- en todas las guardas y en ningún otro lugar, con lo cual se sigue cumpliendo que lo que retorna al aplicarse
-- únicamente con un ingrediente es una función de tipo Persona -> Persona, independientemente de cuál sea el camino a seguir.


{-Definir la función tomarPocion, que recibe el
nombre de una poción y una persona, y devuelve
cómo quedaría la persona después de haber tomado
 la poción, que en general implica que la persona
 consuma los distintos ingredientes de la poción
  y efectos adicionales que la poción mágica
  otorga.-}

  {-
Tomar la poción “Felix Felicis” es equivalente a
consumir “Cuerno de unicornio”, “Ajenjo”, “Menta”,
“Ligústico”, y finalmente aumentar la suerte en100.
-}
tomarPocion :: String -> Persona ->Persona
tomarPocion "Felix Felicis"  =
   aumentarSuerte 100 .
   consumirIngrediente "Cuerno de unicornio" .
   consumirIngrediente "Ajenjo" .
   consumirIngrediente "Menta" .
   consumirIngrediente "Lunguistico"

{-
Tomar la “Veritaserum” es equivalente a consumir
“Pelo de unicornio”, “Corazón de dragón”, “Pluma
de Fénix”, y finalmente deja a la persona con
suerte = 0.
-}
tomarPocion "Veritaserum"  =
   conSuerte 0 .
   consumirIngrediente "Pluma de fenix" .
   consumirIngrediente "Corazon de dragon" .
   consumirIngrediente "Pelo de unicornio"


{-Tomar “Antídoto” es equivalente a consumir
“Bezoar”, “Díctamo”, “Valeriana” y luego reducir
la suerte de la persona en 10.-}
tomarPocion "Antidoto"  =
   aumentarSuerte (-10) .
   consumirIngrediente "Valeriana" .
   consumirIngrediente "Dictamo" .
   consumirIngrediente "Bezoar"


{-
Saber si dos pociones se anulan,
que se cumple si la persona queda igual que
al principio luego de tomar ambas pociones.
-}

seAnulan :: String -> String -> Persona -> Bool
seAnulan pocion1 pocion2 unaPersona =
   ((== unaPersona) .
   tomarPocion pocion1 .
   tomarPocion pocion2 ) unaPersona
-- Al aplicar unaPersona a la composición de (tomarPocion pocion1) y (tomarPocion pocion2)
-- obtenemos una nueva persona resultante que podemos comparar con la persona inicial para corroborar que
-- luego de pasar por los efectos de ambas pociones termina igual que al principio.
-- IMPORTANTE: unaPersona sigue siendo la misma persona, tiene la misma salud y la misma suerte
-- recordar que NO HAY EFECTO DE LADO, asi que aplicar esa persona a tomarPocion no la modifica solo va generando
-- nuevas personas a partir de esa inicial.
