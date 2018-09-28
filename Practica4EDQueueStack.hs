import Practica4EDStack

--7. Queue con dos stack
--Implemente la interfaz de Queue pero en lugar de una lista utilice dos stack.
--La estructura funciona de la siguiente manera. 
--Llamemos a una de las stack fs (front stack) y a la otra bs (back stack).
--Quitaremos elementos a través de fs y agregaremos a través de bs, pero todas 
--las operaciones deben garantizar el siguiente invariante de representación: 
--Si fs se encuentra vacía, entonces la cola se encuentra vacía.
-- ¿Qué ventaja tiene esta representación de Queue con respecto a la de listas?


data Queue a = MxQ (Stack a) (Stack a) 
-- Invariantes de Representacion 
    -- Si fs se encuentra vacía, entonces la cola se encuentra vacía.

--q1 = MxQ [1,2,3,5]
emptyQ :: Queue a  --Orden 
--Crea una cola vacía.
emptyQ = MxQ emptyS emptyS

isEmptyQ :: Queue a -> Bool --Orden 
--Dada una cola indica si la cola está vacía.
isEmptyQ (MxQ fs bs) = isEmptyS fs && isEmptyS bs
     

enQueue :: a -> Queue a -> Queue a --Orden 
--Dados un elemento y una cola, agrega ese elemento a la cola.
enQueue e (MxQ fs bs) = MxQ fs (push e bs)
     

firstQ :: Queue a -> a --Orden 
--Dada una cola devuelve el primer elemento de la cola.
--firstQ (MxQ  bs) = top 
firstQ (MxQ fs bs)  = if isEmptyS fs
                        then top bs
                        else top fs


dequeue :: Queue a -> Queue a --Orden 
--Dada una cola la devuelve sin su primer elemento.
--dequeue (MxQ  []) = error "Lista Vacia" 
dequeue (MxQ fs bs) = if lenS fs == 1
                        then MxQ bs emptyS
                        else MxQ (pop fs) bs