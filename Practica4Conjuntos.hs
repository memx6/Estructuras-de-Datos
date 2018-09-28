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




