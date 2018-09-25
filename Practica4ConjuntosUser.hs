import Practica4Conjuntos

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show

----- Conjuntos -----------
--1. Como usuario del tipo abstracto Set implementar las siguientes funciones:
         
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que
-- pertenecen al conjunto.
losQuePertenecen [] s = []
losQuePertenecen (x:xs) s = if belongs x s
                                then x: losQuePertenecen xs s
                                else losQuePertenecen xs s
         
listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)

--
sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto 
--como estructura auxiliar.
sinRepetidos xs = setToList (listToSet xs)

--2.
unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los
--conjuntos del arbol.
unirTodos EmptyT            = emptyS
unirTodos (NodeT s ss1 ss2) = unionS s (unionS(unirTodos ss1) (unirTodos ss2))


