import Set

--1. Como usuario del tipo abstracto Set implementar las siguientes funciones:
         

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los ele- mentos que
-- pertenecen al conjunto.
losQuePertenecen [] s = 
losQuePertenecen (x:xs) s = if belongs x s
                                then x: losQuePertenecen xs s
                                else losQuePertenecen xs s
         
listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = adds x (listToSet xs)

--
sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto 
--como estructura auxiliar.
sinRepetidos xs = setToList (listToSet xs)

--2.
--unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los
--conjuntos del arbol.
--Implementar la variante del tipo abstracto Set con una lista que no tiene 
--repetidos y guarda la cantidad de elementos en la estructura.
--Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas
--las operaciones de esta implementación, pero para mantener una interfaz común
--entre distintas posibles implementaciones estamos obligados a escribir así los tipos.