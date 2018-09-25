module Practica4Conjuntos (
                Set, emptyS, 
                addS, 
                belongs, 
                sizeS, 
                removeS, 
                unionS, 
                intersectionS, 
                setToList,
                ) where
--1. Conjunto
--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones: 

data Set a = S [a] deriving (Show ,Eq)
--Invariantes de Representacion:

emptyS :: Set a
--Crea un conjunto vacío.
emptyS = S []
     

addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS e (S []) = S [e] 
addS e (S xs) = if elem e xs
                    then S (e:xs)
                    else S xs
     

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs e (S []) = False 
belongs e (S xs) = elem e xs
    

sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = length xs
     

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS e (S []) = S []
removeS e (S xs) = S (eliminarElemento e xs)
     
eliminarElemento :: Eq a=> a ->[a]-> [a]
eliminarElemento e [] = []
eliminarElemento e (x:xs) = if e == x
                                then eliminarElemento e xs
                                else x:eliminarElemento e xs 


unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs) (S ys) = S (sacarRepetidos (xs ++ ys))

sacarRepetidos :: Eq a => [a]-> [a]
sacarRepetidos [] = []
sacarRepetidos (x:xs) = if (apariciones x xs) == 0
                            then x: sacarRepetidos xs
                            else sacarRepetidos xs


apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones n [] = 0
apariciones n (x:xs) = 
                    if n == x
                        then 1 + apariciones n xs
                        else 0 + apariciones n xs


intersectionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos en común entre ambos.
intersectionS (S xs) (S ys) = S(intersectionB xs ys)

intersectionB :: Eq a=> [a]->[a]->[a]
intersectionB [] []     = []
intersectionB (x:xs) ys = if elem x ys
                            then x:intersectionB xs ys
                            else intersectionB xs ys 

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S (x:xs)) = xs




--------ANEXO------------
--1. Maybe
--1. Defina las siguientes operaciones (notar que son parciales si no devuelven Maybe):
--a) headM :: [a] -> Maybe a Versión de head que es total


--b) lastM :: [a] -> Maybe a
--Dada una lista, devuelve su último elemento.


--c) maximumM :: Ord a => [a] -> Maybe a
--Dada una lista de elementos devuelve el máximo.


--d) initM :: [a] -> Maybe [a]
--Dada una lista quita su último elemento.


--e) get :: Int -> [a] -> Maybe a
--Dado un índice devuelve el elemento de esa posición.


--f) indiceDe :: Eq a => a -> [a] -> Maybe Int
--Dado un elemento y una lista devuelve la posición de la lista en la que se encuentra dicho elemento.


--g) lookupM :: Eq k => [(k,v)] -> k -> Maybe v
--Dada una lista de pares (clave, valor) y una clave devuelve el valor asociado a la clave.


--h) fromJusts :: [Maybe a] -> [a]
--Devuelve los valores de los Maybe que no sean Nothing.


--i) minT :: Ord a => Tree a -> Maybe a 
--Dado un árbol devuelve su elemento mínimo.


--2. Indicar los ordenes de complejidad en peor caso de cada función imple- mentada.






