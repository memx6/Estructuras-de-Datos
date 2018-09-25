module Practica4EDStack (Stack2,
    emptyS2,
    isEmptyS2,
    push2,
    pop2,
    top2,
    lenS2,
    maxS2
) where
    
--4.
--Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out).
-- Esto significa que los últimos elementos agregados a la estructura son los primeros 
 --en salir (como en una pila de platos). Su interfaz es la siguiente:
data Stack a = S [a]

emptyS :: Stack a
 --Crea una pila vacía.
emptyS = S [] 
 
      
isEmptyS :: Stack a -> Bool
 --Dada una pila indica si está vacía.
isEmptyS (S []) = True
isEmptyS (S xs) = False
      
 
push :: a -> Stack a -> Stack a
 --Dados un elemento y una pila, agrega el elemento a la pila.
push e (S xs ) = S (e:xs) 

 
top :: Stack a -> a
 --Dada un pila devuelve el elemento del tope de la pila.
top (S xs) = head xs
      
 
pop :: Stack a -> Stack a
 --Dada una pila devuelve la pila sin el primer elemento.
pop (S xs ) = (S (tail xs))
 
lenS :: Stack a -> Int
lenS (S xs ) = length xs

--2. Implementar el tipo abstracto Stack utilizando listas.-----
     
data Stack2 a = S2 [a] Int [a]

-- Inv de Rep
-- Sea S xs n ms
----- n es el largo de xs
----- n es el largo de ms
----- el head de ms es el maximum de xs

emptyS2 :: Stack2 a
--Crea una pila vacía.
emptyS2 = S2 [] 0 []

     
isEmptyS2 :: Stack2 a -> Bool
--Dada una pila indica si está vacía.
isEmptyS2 (S2 xs n ms) = n == 0
     

push2 :: Ord a => a -> Stack2 a -> Stack2 a
--Dados un elemento y una pila, agrega el elemento a la pila.
push2 e (S2 xs n ms) = S2 (e:xs) (n+1) (actualizar2 e ms)

actualizar2 x [] = [x]
actualizar2 x (m:ms) = max x m : m : ms 
     

top2 :: Stack2 a -> a
--Dada un pila devuelve el elemento del tope de la pila.
top2 (S2 xs n ms) = head xs
     

pop2 :: Stack2 a -> Stack2 a
--Dada una pila devuelve la pila sin el primer elemento.
pop2 (S2 xs n ms) = (S2 (tail xs)(n-1)(tail ms))

lenS2 :: Stack2 a -> Int
lenS2 (S2 xs n ms) = n

-- O(1)
maxS2 :: Ord a => Stack2 a -> a
maxS2 (S2 xs n ms) = head ms
