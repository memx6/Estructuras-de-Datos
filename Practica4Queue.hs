module Practica4Queue (Queue, 
                        emptyQ,
                        isEmptyQ,
                        enQueue,
                        firstQ,
                        dequeue,
                        Queue1, 
                        emptyQ1,
                        isEmptyQ1,
                        enQueue1,
                        firstQ1,
                        dequeue1) where
--3. Queue (cola)
--Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). 
--Esto significa que los elementos salen en el orden con el que entraron, es decir, 
--el que se agrega primero es el primero en salir (como la cola de un banco). 
--Su interfaz es la siguiente:
     
data Queue a = MxQ [a] deriving (Show,Eq)
-- Invariantes de Representacion 
    -- el Primero en entrar es el primero en salir
    -- el enQueue agrega el elemento al principio de la lista "5" [5,4,3,2,1]
    -- el dequeue saca el ultimo elemento de la lista, que seria el primero en entrar

q1 = MxQ [1,2,3,5]
emptyQ :: Queue a  --Orden 1
--Crea una cola vacía.
emptyQ = MxQ []

isEmptyQ :: Queue a -> Bool --Orden 1
--Dada una cola indica si la cola está vacía.
isEmptyQ (MxQ []) = True
isEmptyQ _ = False
     

enQueue :: a -> Queue a -> Queue a --Orden 1
--Dados un elemento y una cola, agrega ese elemento a la cola.
enQueue e (MxQ xs) = MxQ (e:xs)
     

firstQ :: Queue a -> a --Orden N
--Dada una cola devuelve el primer elemento de la cola.
firstQ (MxQ [x]) = x
firstQ (MxQ xs)  = last xs


dequeue :: Queue a -> Queue a --Orden N 
--Dada una cola la devuelve sin su primer elemento.
dequeue (MxQ []) = error "Lista Vacia" 
dequeue (MxQ xs) = MxQ (init xs)


----- Segunda Version -------
data Queue1 a = MxQ1 [a] deriving (Show,Eq)
-- Invariantes de Representacion 
    -- el Primero en entrar es el primero en salir
    -- el enQueue agrega el elemento al final de la lista "5" [1,2,3,4,5]
    -- el dequeue saca el primer elemento de la lista, que seria el primero en entrar

q5 = MxQ1 [1,2,3,5]
emptyQ1 :: Queue1 a  --Orden 1
--Crea una cola vacía.
emptyQ1 = MxQ1 []

isEmptyQ1 :: Queue1 a -> Bool --Orden 1
--Dada una cola indica si la cola está vacía.
isEmptyQ1 (MxQ1 []) = True
isEmptyQ1 _ = False
     

enQueue1 :: a -> Queue1 a -> Queue1 a --Orden n
--Dados un elemento y una cola, agrega ese elemento a la cola.
enQueue1 e (MxQ1 xs) = MxQ1 (xs ++ [e])
     

firstQ1 :: Queue1 a -> a --Orden 1
--Dada una cola devuelve el primer elemento de la cola.
firstQ1 (MxQ1 [x]) = x
firstQ1 (MxQ1 xs)  = head xs


dequeue1 :: Queue1 a -> Queue1 a --Orden 1
--Dada una cola la devuelve sin su primer elemento.
dequeue1 (MxQ1 []) = error "Lista Vacia" 
dequeue1 (MxQ1 xs) = MxQ1 (tail xs)