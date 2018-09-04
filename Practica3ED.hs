--1.
--Árboles binarios
--Defina las siguientes funciones sobre árboles binarios utilizando recursión 
--estructural según corresponda:
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show


a1= EmptyT
a2 = NodeT 4 (a1)(a3)
a3 = NodeT 6 (a1)(a1)
a4= (NodeT 2 (a3)(a1))

a5 = NodeT "Hola" (a1)(a6)
a6 = NodeT "Mauro" (a1)(a1)
a7= (NodeT "ED" (a6)(a1))

--1. 
sumarT :: Tree Integer -> Integer
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT          = 0
sumarT (NodeT a t1 t2) = a + sumarT t1 + sumarT t2


--2. 
sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño 
--del árbol (size en inglés).
sizeT EmptyT          = 0
sizeT (NodeT a t1 t2) = 1 + sizeT t1 + sizeT t2


--3. 
mapDobleT :: Tree Integer -> Tree Integer
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT a t1 t2) = NodeT(a*2)  
                                (mapDobleT t1)
                                ( mapDobleT t2)


--4. 
mapLongitudT :: Tree String -> Tree Int
--Dado un árbol de palabras devuelve un árbol con la longitud de cada palabra.
mapLongitudT EmptyT          = EmptyT
mapLongitudT (NodeT s t1 t2) = NodeT (length s) 
                                    (mapLongitudT t1) 
                                    (mapLongitudT t2)


--5. 
perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual 
--a ese en el árbol.
perteneceT e EmptyT          = False
perteneceT e (NodeT a t1 t2) =  e == a || perteneceT e t1 || (perteneceT e t2)


--6. 
aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol 
--que son iguales a e.
aparicionesT e EmptyT          = 0
aparicionesT e (NodeT a t1 t2) = if e == a 
                                    then 1 + aparicionesT e t1 + aparicionesT e t2
                                    else aparicionesT  e t1  + aparicionesT e t2

------- 7 ----
data Persona = P String Int deriving Show

p1 = P "Mauro" 23
p2 = P "Fede" 25

a8 = NodeT p1 (a1)(a9)
a9 = NodeT p2 (a1)(a1)
a10= (NodeT p1 (a9)(a1))
                  
edad :: Persona -> Int
--Devuelve la edad de una persona
edad (P n e) = e

sumatoria :: [Persona] -> Int
sumatoria [] = 0
sumatoria (x:xs) = edad x + sumatoria xs

promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. La lista 
--al menos posee una persona.
promedioEdad [] = 0
promedioEdad xs = div (sumatoria xs) (length xs)

--7. 
promedioEdadesT :: Tree Persona -> Int
--Dado un árbol de personas devuelve el promedio entre las edades de todas las 
--personas. Definir las subtareas que sean necesarias para resolver esta función.
--Nota: Utilizar el tipo Persona ya definido.
promedioEdadesT EmptyT          = 0
--promedioEdadesT (NodeT p t1 t2) = promedioEdadesT (promedioEdad t1) + promedioEdadesT (promedioEdad t2)

---------

--8. contarLeaves :: Tree a -> Int
--Dado un árbol devuelve su cantidad de hojas.
--Nota: una hoja (leaf en inglés) es un nodo que no tiene hijos.


--9. leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.


--10. heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura (height en inglés) de un árbol es la cantidad máxima de nodos 
--entre la raíz y alguna de sus hojas. La altura de un árbol vacío es cero y la de 
--una hoja es 1.


--11. contarNoHojas :: Tree a -> Int
--Dado un árbol devuelve el número de nodos que no son hojas. ¿Cómo podría resolverla
--sin utilizar recursión? Primero defínala con recursión y después sin ella.


--12. espejoT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el 
--derecho, en cada nodo del árbol.


--13. listInOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--in-order. Nota: En el modo in-order primero se procesan los elementos del hijo 
--izquierdo, luego la raiz y luego los elementos del hijo derecho.


--14. listPreOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--pre-order. Nota: En el modo pre-order primero se procesa la raiz, luego los elementos 
--del hijo izquierdo, a continuación los elementos del hijo derecho.


--15. listPosOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--post- order.
--Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, 
--a continuación los elementos del hijo derecho y finalmente la raiz.


--16. concatenarListasT :: Tree [a] -> [a]
--Dado un árbol de listas devuelve la concatenación de todas esas listas. El recorrido 
--debe ser in-order.


--17. levelN :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. Nota: 
--El primer nivel de un árbol (su raíz) es 0.


--18. listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un 
--nivel de dicho árbol.


--19. widthT :: Tree a -> Int
--Dado un árbol devuelve su ancho (width en inglés), que es la cantidad de nodos del
--nivel con mayor cantidad de nodos.


--20. ramaDerecha :: Tree a -> [a]
--Devuelve todos los elementos encontrados en el camino de todas las ramas derechas.


--21. ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol


--22. todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta 
--las hojas.