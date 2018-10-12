module Practica5EDBST (Tree ,
                    isEmptyT,
                    perteneceBST,
                    insertBST,
                    minBST,
                    maxBST,
                    deleteBST,
                    deleteMinBST,
                    deleteMaxBST
                    ) where
--3. Arboles de Busqueda
--Ejercicio 6
--Implementar las siguientes funciones suponiendo que reciben un  
--arbol binario que cumple los invariantes de BST. Todas deben ser 
--implementadas en O(log n).

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show
-- Inv. Rep
--- Es BST (Rama izq menor que la Rama Derecha)
--- avl |height t1| - |height t2| <= 1 Balanceado

data MaybeM a = JustM a
              | NothingM deriving (Show, Eq)


isEmptyT :: Tree a -> Bool
isEmptyT EmptyT = True
isEmptyT _      =  False

--1. 
perteneceBST :: Ord a => a -> Tree a -> Bool 
--Dado un BST dice si el elemento pertenece o no al arbol.
perteneceBST e EmptyT          = False
perteneceBST e (NodeT a t1 t2) =  if e == a
                                    then True
                                    else if e < a
                                        then perteneceBST e t1
                                        else perteneceBST e t2
                                    
--2. 
insertBST :: Ord a => a -> Tree a -> Tree a 
--Dado un BST inserta un elemento en el arbol.
insertBST e EmptyT          = (NodeT e EmptyT EmptyT)
insertBST e (NodeT x t1 t2) = if e < x
                                then NodeT e (insertBST e t1) t2
                                else NodeT e t1 (insertBST e t2)

--3. 
minBST :: Ord a => Tree a -> a
--Dado un BST devuelve el mınimo elemento.
minBST EmptyT          = error "No hay minimo en un arbol vacio"
minBST (NodeT a t1 t2) = if isEmptyT t1
                            then a
                            else minBST t1

--4. 
deleteMinBST :: Ord a => Tree a -> Tree a
--Dado un BST devuelve el arbol sin el mınimo elemento.
deleteMinBST EmptyT           = EmptyT
deleteMinBST (NodeT  x t1 t2) = if isEmptyT t1  
                                    then t2
                                    else (NodeT x (deleteMinBST t1) t2 )


--5. 
maxBST :: Ord a => Tree a -> a
--Dado un BST devuelve el maximo elemento.
maxBST EmptyT           = error "no hay elementos en el arbol"
maxBST (NodeT  x t1 t2) = if isEmptyT t2  
                                    then x
                                    else maxBST t2 


--6. 
deleteMaxBST :: Ord a => Tree a -> Tree a
--Dado un BST devuelve el arbol sin el maximo elemento
deleteMaxBST EmptyT          = EmptyT
deleteMaxBST (NodeT a t1 t2) = if isEmptyT t2 
                                    then t1
                                    else NodeT a t1 (deleteMaxBST t2)


--7. 
deleteBST :: Ord a => a -> Tree a -> Tree a 
--Dado un BST borra un elemento en el arbol.
deleteBST e EmptyT          = EmptyT
deleteBST e (NodeT a t1 t2) = if e == a
                                then join t1 t2
                                else if e < a
                                    then (NodeT a (deleteBST e t1) t2)
                                    else (NodeT a t1 (deleteBST e t2))

join :: Ord a => Tree a -> Tree a -> Tree a
join t1 EmptyT = t1
join t1 t2     = NodeT (minBST t2) t1 (deleteMinBST t2) 

--8. 
splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Dado un BST devuelve un par con el m ınimo elemento y el  arbol sin 
--el mismo.
splitMinBST EmptyT = error "error"
splitMinBST t = ((minBST t), (deleteBST (minBST t)t ))


--9. 
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Dado un BST devuelve un par con el maximo elemento y el  arbol sin 
--el mismo.
splitMaxBST EmptyT = error "error"
splitMaxBST t = ((maxBST t), (deleteBST (maxBST t) t))



--10. 
esBST :: Ord a => Tree a -> Bool
--Dado un  arbol cualquiera indica si cumple con las condiciones de BST. 
--Nota: esta funcion no se suele usar en ninguna implementacion porque 
--es costosa.
esBST EmptyT          = True
esBST (NodeT x t1 t2) = sonTodosMenores x t1 && sonTodosMayores x t2 
                        && esBST t1 && esBST t2


sonTodosMenores :: Ord a => a -> Tree a -> Bool
sonTodosMenores _ EmptyT         = True
sonTodosMenores a (NodeT x t1 _) = if x < a
                                        then True && sonTodosMenores a t1
                                        else False
                                         

sonTodosMayores :: Ord a => a -> Tree a -> Bool
sonTodosMayores _ EmptyT         = True
sonTodosMayores a (NodeT x _ t2) = if x > a
                                        then sonTodosMayores a t2
                                        else False

--11. 
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Dado un BST y un elemento, devuelve el maximo elemento que sea menor o 
--igual al elemento dado.
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA a (NodeT x t1 t2) = if x == a
                                        then Just x
                                        else if x < a
                                                then elMaximoMenorA a t1
                                                else elMaximoMenorA a t2


--12. 
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Dado un BST y un elemento, devuelve el mınimo elemento que sea mayor o 
--igual al elemento dado.
elMinimoMayorA _ EmptyT = Nothing
elMinimoMayorA a (NodeT x t1 t2) = if x == a
                                    then Just x
                                    else if x > a
                                            then elMinimoMayorA a t2
                                            else elMinimoMayorA a t1

