import Practica4Queue
------- QUEUE -----------
--Como usuario del tipo abstracto Queue implementar las siguientes funciones:
q2 = enQueue "Fede" (enQueue "Mauro"(emptyQ))  
q3 = enQueue "F" (enQueue "M"(emptyQ))  
largoQ :: Queue a -> Int -- Orden n.n
--Cuenta la cantidad de elementos de la cola.
largoQ q = if isEmptyQ q
                then 0
                else 1 + largoQ (dequeue q)
    

atender :: Queue String -> [String] -- Orden n.n
--Dada una cola de nombres de personas, devuelve la lista de los mismos,
--donde el orden de la lista es el de la cola.
atender q = if isEmptyQ q
                then []
                else reverse(firstQ q : atender (dequeue q))

    

unirQ :: Queue a -> Queue a -> Queue a -- Orden n.n
--Inserta todos los elementos de la segunda cola en la primera.
unirQ q1 q2 = if isEmptyQ q2
                    then q1
                    else enQueue (firstQ q2) (unirQ q1 (dequeue q2))

------- QUEUE 2 -----------
--Como usuario del tipo abstracto Queue implementar las siguientes funciones:
q4 = enQueue1 1 (enQueue1 2(emptyQ1))  
q6 = enQueue1 "F" (enQueue1 "M"(emptyQ1))  

largoQ1 :: Queue1 a -> Int -- Orden n.n
--Cuenta la cantidad de elementos de la cola.
largoQ1 q = if isEmptyQ1 q
                then 0
                else 1 + largoQ1 (dequeue1 q)
    

atender1 :: Queue1 String -> [String] -- Orden n.n
--Dada una cola de nombres de personas, devuelve la lista de los mismos,
--donde el orden de la lista es el de la cola.
atender1 q = if isEmptyQ1 q
                then []
                else reverse(firstQ1 q : atender1 (dequeue1 q))

    

unirQ1 :: Queue1 a -> Queue1 a -> Queue1 a -- Orden n.n
--Inserta todos los elementos de la segunda cola en la primera.
unirQ1 q1 q2 = if isEmptyQ1 q2
                    then q1
                    else enQueue1 (firstQ1 q2) (unirQ1 q1 (dequeue1 q2))