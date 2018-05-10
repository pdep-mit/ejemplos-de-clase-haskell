-- Enunciado: https://docs.google.com/document/d/1x74viDWXSV915OxiAV9W5xUw8AqelOUopTYHKzwPcmE
data Depto = UnDepto {
    ambientes :: Int,
    superficie :: Int, 
    precio :: Int,
    barrio :: String
} deriving Show


ordenarSegun:: (a->a->Bool)->[a]->[a]
ordenarSegun _ [] = []
ordenarSegun anteriorA (x:xs)=
    ordenadosQueCumplen anteriorA (`anteriorA` x) xs 
    ++ [x] ++
    ordenadosQueCumplen anteriorA (not.(`anteriorA` x)) xs
        where ordenadosQueCumplen anteriorA condicion  = ordenarSegun anteriorA.filter condicion
-- la funcion ordenadosQueCumplen usaba point free todo el tiempo

between:: Ord a => a -> a -> a -> Bool
between x y z = 
    x<=z && y >=z

deptosDeEjemplo = [
    UnDepto 3 80 7500 "Palermo",
    UnDepto 1 45 3500 "Villa Urquiza",
    UnDepto 2 50 5000 "Palermo",
    UnDepto 1 45 5500 "Recoleta"]

--1)
--a
mayor::Ord b=>(a->b)->a->a->Bool
mayor funcion valor1 valor2 =
    funcion valor1 > funcion valor2

menor::Ord b=>(a->b)->a->a->Bool
menor funcion valor1 valor2 =
        funcion valor1 < funcion valor2
    

--b

    --ordenarSegun:: (a->a->Bool)->[a]->[a]
    --ordenarSegun (mayor length) ["holaa","chau"]

--2)

ubicadoEn::[String] -> Depto -> Bool
ubicadoEn barrios departamento = 
    any (estaEn departamento) barrios

estaEn departamento elBarrio =
    barrio departamento == elBarrio

ubicadoEn' barrios =
   -- elem (barrio departamento) barrios
    flip elem barrios.barrio

--b
    --between:: Ord a => a -> a -> a -> Bool
cumpleRango::Ord a => (Depto->a)->a->a-> Depto -> Bool 
cumpleRango funcion cotaInf cotaSup =
    between cotaInf cotaSup .funcion
    --between cotaInf cotaSup (funcion depto)

--3)
--a
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

--cumpleBusqueda:: [Requisito] -> Depto -> Bool
cumpleBusqueda:: Busqueda -> Depto -> Bool

cumpleBusqueda requisitos departamento =
    all (cumpleRequisito departamento) requisitos 

cumpleRequisito departamento requisito = requisito departamento

--cumpleBusqueda [ubicadoEn'["Palermo","San Miguel"] ,cumpleRango precio 1000 7000 ] departamentito


--b

type CriterioDeOrd = Depto->Depto->Bool

departamentosBuscadosEnOrdenDeInteres::  Busqueda -> CriterioDeOrd -> [Depto] -> [Depto]
departamentosBuscadosEnOrdenDeInteres requisitos criterioDeOrdenamiento =
        ordenarSegun criterioDeOrdenamiento
          .filter (cumpleBusqueda requisitos)

--c

consulta3c deptosDeEjemplo =
    departamentosBuscadosEnOrdenDeInteres 
    [ubicadoEn'["Recoleta","Palermo"],
        cumpleRango ambientes 1 2,
        (<6000).precio]
    (mayor superficie)
    deptosDeEjemplo

--4)
data Persona = UnaPersona {
    mail::String,
    busquedas::[Busqueda]
}
mailsDePersonasInteresadas:: Depto -> [Persona] -> [String]
mailsDePersonasInteresadas departamento  =
    map mail.filter (leInteresa departamento) 

leInteresa::Depto -> Persona -> Bool
leInteresa departamento =
    any (flip cumpleBusqueda departamento).busquedas 

--5) :t f te da la solucion
f x y = y.head.map(\(_,z)-> menor x z).filter (even.fst)
