import Practica5EDBST
--4. Colas de Prioridad (Heaps)
--Ejercicio 8
--Implementar la interfaz de Heap utilizando un  arbol binario con 
--invariantes de BinaryHeaps (como se vio en clase):

data Heap a = H (Tree a)
    -- Inv. Rep
    --- Es BST

emptyH :: Heap a
emptyH = H EmptyT
     
isEmptyH :: Heap a -> Bool
isEmptyH (H EmptyT) = True
isEmptyH _        = False
     

insertH :: Ord a => a -> Heap a -> Heap a
insertH e (H t) = H (insertBST e t)
     

findMin :: Ord a => Heap a -> a -- Parcial en emptyH
findMin (H t) = H minBST t
--findMin (NodeT x EmptyT t2) = x
--findMin (NodeT x t1 t2)     = findMin t2


deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH
deleteMin (H t) = H deleteMinBST t
--deleteMin (NodeT x EmptyT t2) = t2
--deleteMin (NodeT x t1 t2)     = Node x(deleteMin t1) t2
    
--splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH
--splitMin (H NodeT x EmptyT t2) = (x, t2)
--splitMin (H NodeT x t1 t2)     = 
--                                case splitMin t1 of
--                                    (m, t1m) -> (m, NodeT x t1m t2)
                            --armar x (splitMin t1) t2 ????

--armar x (m,t1) t2 = (m, NodeT x t1 t2)



--Indicar los costos de cada operaci ́on.
--Ejercicio 10
--Implementar la funcion heapSort :: Ord a => [a] -> [a], que dada una 
--lista la ordena de menor a mayor utilizando una Heap como estructura 
--auxiliar.