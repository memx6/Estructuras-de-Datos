--1.
--Árboles binarios
--Defina las siguientes funciones sobre árboles binarios utilizando recursión 
--estructural según corresponda:
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show


a1= EmptyT
a2 = NodeT 4 (a0)(a3)
a3 = NodeT 6 (a1)(a1)
a4= (NodeT 2 (a1)(a3))
a0 = (NodeT 9 a1 (NodeT 5 a1 a99))
a99 = (NodeT 7 a1 a1)

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


sumaEdad :: Tree Persona -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. La lista 
--al menos posee una persona.
sumaEdad EmptyT = 0
sumaEdad (NodeT p t1 t2) = (edad p) + (sumaEdad t1) + (sumaEdad t2)

--7. 
promedioEdadesT :: Tree Persona -> Int
--Dado un árbol de personas devuelve el promedio entre las edades de todas las 
--personas. Definir las subtareas que sean necesarias para resolver esta función.
--Nota: Utilizar el tipo Persona ya definido.
promedioEdadesT tree = div (sumaEdad tree) (sizeT tree)


--8. 
contarLeaves :: Tree a -> Int
--Dado un árbol devuelve su cantidad de hojas.
--Nota: una hoja (leaf en inglés) es un nodo que no tiene hijos.
contarLeaves EmptyT                  = 0
contarLeaves (NodeT a EmptyT EmptyT) = 1
contarLeaves (NodeT a t1 t2)         = (contarLeaves t1) + (contarLeaves t2)


--9. 
leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT                  = []
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT a t1 t2)         = (leaves t1) ++ (leaves t2)


--10. 
heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura (height en inglés) de un árbol es la cantidad máxima de nodos 
--entre la raíz y alguna de sus hojas. La altura de un árbol vacío es cero y la de 
--una hoja es 1.
heightT EmptyT          = 0
heightT (NodeT a t1 t2) = 1 + (max (heightT t1) (heightT t2))


--11. 
contarNoHojas :: Tree a -> Int
--Dado un árbol devuelve el número de nodos que no son hojas. ¿Cómo podría resolverla
--sin utilizar recursión? Primero defínala con recursión y después sin ella.
contarNoHojas EmptyT                  = 0
contarNoHojas (NodeT a EmptyT EmptyT) = 0
contarNoHojas (NodeT a t1 t2)         = 1 + contarNoHojas t1 + contarNoHojas t2


--12. 
espejoT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el 
--derecho, en cada nodo del árbol.
espejoT EmptyT          = EmptyT
espejoT (NodeT a t1 t2) = (NodeT a (espejoT t2)(espejoT t1))


--13. 
listInOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--in-order. Nota: En el modo in-order primero se procesan los elementos del hijo 
--izquierdo, luego la raiz y luego los elementos del hijo derecho.
listInOrder EmptyT          = []
listInOrder (NodeT a t1 t2) = (listInOrder t1)++ [a] ++(listInOrder t2)


--14. 
listPreOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--pre-order. Nota: En el modo pre-order primero se procesa la raiz, luego los elementos 
--del hijo izquierdo, a continuación los elementos del hijo derecho.
listPreOrder EmptyT          = []
listPreOrder (NodeT a t1 t2) = [a] ++ (listPreOrder t1) ++ (listPreOrder t2)


--15. 
listPosOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--post- order.
--Nota: En el modo post-order primero se procesan los elementos del hijo izquierdo, 
--a continuación los elementos del hijo derecho y finalmente la raiz.
listPosOrder EmptyT          = []
listPosOrder (NodeT a t1 t2) = (listPosOrder t1) ++ (listPosOrder t2) ++ [a]


--16. 
concatenarListasT :: Tree [a] -> [a]
--Dado un árbol de listas devuelve la concatenación de todas esas listas. El recorrido 
--debe ser in-order.
concatenarListasT EmptyT          = []
concatenarListasT (NodeT as t1 t2)= (concatenarListasT t1) ++ as ++ (concatenarListasT t2)


--17. 
levelN :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. Nota: 
--El primer nivel de un árbol (su raíz) es 0.
levelN n EmptyT          = []
levelN n (NodeT a t1 t2) = if n == 0
                                then [a]
                                else (levelN (n-1) t1) ++ (levelN (n-1) t2) 


--18. 
listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un 
--nivel de dicho árbol.
listPerLevel EmptyT          = []
listPerLevel (NodeT a t1 t2) = [a] : listarNiveles (listPerLevel t1)(listPerLevel t2)


listarNiveles :: [[a]] -> [[a]] -> [[a]]
listarNiveles [] []        = []
listarNiveles xs []        = xs
listarNiveles [] ys        = ys
listarNiveles (x:xs)(y:ys) = (x ++ y) : listarNiveles xs ys


--19. 
--widthT :: Tree a -> Int
--Dado un árbol devuelve su ancho (width en inglés), que es la cantidad de nodos del
--nivel con mayor cantidad de nodos.
--widthT EmptyT          = 0
--widthT (NodeT a t1 t2) = 


--20. 
ramaDerecha :: Tree a -> [a]
--Devuelve todos los elementos encontrados en el camino de todas las ramas derechas.
ramaDerecha EmptyT          = []
ramaDerecha (NodeT a t1 t2) = [a] ++ ramaDerecha t2


--21. 
ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT a t1 t2) = if (heightT t1) > (heightT t2) 
                                    then [a] ++ (ramaMasLarga t1) 
                                    else [a] ++ ramaMasLarga t2


--22. 
todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta 
--las hojas.
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT a EmptyT EmptyT) = [[a]]
todosLosCaminos (NodeT a t1 t2)         = (todosLosCaminos t1) ++ (todosLosCaminos t2)


agregar :: a -> [[a]] -> [[a]]
agregar e []     = []
agregar e (x:xs) = (e:x) : agregar e xs


--2. Mapa de tesoros
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
data Dir = Izq | Der deriving (Show,Eq)
data Objeto = Tesoro | Chatarra deriving (Show,Eq)
data Mapa = Cofre Objeto
          | Bifurcacion Objeto Mapa Mapa deriving (Show,Eq)


map1 = Bifurcacion Chatarra map2 map3
map2 = Bifurcacion Chatarra map3 (Cofre Chatarra)
map3 = Cofre Tesoro
--Definir las siguientes operaciones:
--1. 
hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Cofre o)            = esTesoro o
hayTesoro (Bifurcacion x m1 m2)= x == Tesoro || hayTesoro m1 || hayTesoro m2

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

--2. 
hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final del camino es la lista vacía de direcciones.
hayTesoroEn [] (Cofre o)             = esTesoro o
hayTesoroEn (x:xs) (Cofre o)         = False -- Caso en donde la lista dir puede ser mas grande que el mapa 
hayTesoroEn [] (Bifurcacion obj _ _) = esTesoro obj
hayTesoroEn (x:xs) mapa              = hayTesoroEn xs (avanzarEnDir x mapa )


avanzarEnDir :: Dir -> Mapa -> Mapa
avanzarEnDir Der (Bifurcacion o m1 m2) = m2
avanzarEnDir Izq (Bifurcacion o m1 m2) = m1
avanzarEnDir _ (Cofre obj) = (Cofre obj)

--3. 
caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. Precondición: hay un sólo tesoro en el mapa.
caminoAlTesoro (Cofre o)             = []
caminoAlTesoro (Bifurcacion o m1 m2) = if esTesoro o
                                            then []
                                            else if hayTesoro m1
                                                        then Izq : caminoAlTesoro m1
                                                        else Der : caminoAlTesoro m2


--4. 
caminoRamaMasLarga :: Mapa -> [Dir] 
--Indica el camino de la rama más larga.
caminoRamaMasLarga (Cofre o)             = []
caminoRamaMasLarga (Bifurcacion o m1 m2) = if (length (caminoRamaMasLarga m1)) > (length (caminoRamaMasLarga m2))
                                                then Izq : caminoRamaMasLarga m1
                                                else Der : caminoRamaMasLarga m2


--5. 
tesorosPerLevel :: Mapa -> [[Objeto]] 
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPerLevel (Cofre o)             = agregarObjeto o []
tesorosPerLevel (Bifurcacion o m1 m2) = [o] : listarNiveles (tesorosPerLevel m1) (tesorosPerLevel m2)


agregarObjeto x xs = if esTesoro x
                        then [x]:xs
                        else xs

--6. 
todosLosCaminos1 :: Mapa -> [[Dir]] 
--Devuelve todos lo caminos en el mapa.
todosLosCaminos1 (Cofre x)               = []
--todosLosCaminos1 (Bifurcacion a EmptyT EmptyT) = [[a]]
todosLosCaminos1 (Bifurcacion x m1 m2)   = (agregar Izq (todosLosCaminos1 m1)) ++ (agregar Der (todosLosCaminos1 m2))


--3.
--Mapa de tesoros: la revancha
--Ahora el mapa de tesoros tiene en los cofres una lista de objetos, realice las funciones del punto anterior teniendo en cuenta esto.
data Mapa1 = Cofre1 [Objeto]
          | Bifurcacion1 [Objeto] Mapa1 Mapa1
          
map10 = Bifurcacion1 [Chatarra] map20 map30
map20 = Bifurcacion1 [Chatarra] map30 (Cofre1 [Tesoro])
map30 = Cofre1 [Tesoro, Chatarra]
--1. 
hayTesoro1 :: Mapa1 -> Bool
--  Indica si hay un tesoro en alguna parte del mapa.
hayTesoro1 (Cofre1 obj)            = hayTesoros obj
hayTesoro1 (Bifurcacion1 obj m1 m2)= hayTesoros obj || hayTesoro1 m1 || hayTesoro1 m2

hayTesoros :: [Objeto] -> Bool
hayTesoros []      = False
hayTesoros (o:obs) = esTesoro o || hayTesoros obs

--2. 
hayTesoroEn1 :: [Dir] -> Mapa1 -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final del camino es la lista vacía de direcciones.
hayTesoroEn1 [] (Cofre1 obj)           = hayTesoros obj
hayTesoroEn1 (x:xs) (Cofre1 obj)       = False -- Caso en donde la lista dir puede ser mas grande que el mapa 
hayTesoroEn1 [] (Bifurcacion1 obj _ _) = hayTesoros obj
hayTesoroEn1 (x:xs) mapa               = hayTesoroEn1 xs (avanzarEnDir1 x mapa )


avanzarEnDir1 :: Dir -> Mapa1 -> Mapa1
avanzarEnDir1 Der (Bifurcacion1 o m1 m2) = m2
avanzarEnDir1 Izq (Bifurcacion1 o m1 m2) = m1
avanzarEnDir1 _ (Cofre1 obj)             = (Cofre1 obj)
--3. 
caminoAlTesoro1 :: Mapa1 -> [Dir]
--Indica el camino al tesoro. Precondición: hay un sólo tesoro en el mapa.
caminoAlTesoro1 (Cofre1 obj)             = []
caminoAlTesoro1 (Bifurcacion1 obj m1 m2) = if hayTesoros obj
                                            then []
                                            else if hayTesoro1 m1
                                                        then Izq : caminoAlTesoro1 m1
                                                        else Der : caminoAlTesoro1 m2

--4. 
caminoRamaMasLarga1 :: Mapa1 -> [Dir] 
--Indica el camino de la rama más larga.
caminoRamaMasLarga1 (Cofre1 obj)             = []
caminoRamaMasLarga1 (Bifurcacion1 obj m1 m2) = if (length (caminoRamaMasLarga1 m1)) > (length (caminoRamaMasLarga1 m2))
                                                then Izq : caminoRamaMasLarga1 m1
                                                else Der : caminoRamaMasLarga1 m2

--5. 
tesorosPerLevel1 :: Mapa1 -> [[Objeto]] 
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPerLevel1 (Cofre1 obj)             = agregarObjeto1 obj []
tesorosPerLevel1 (Bifurcacion1 obj m1 m2) = (filtrarTesoros obj) : listarNiveles (tesorosPerLevel1 m1) (tesorosPerLevel1 m2)


agregarObjeto1 xs xss = if hayTesoros xs
                        then (filtrarTesoros xs):xss
                        else xss

filtrarTesoros :: [Objeto] -> [Objeto]
filtrarTesoros []     = []
filtrarTesoros [x]    = if esTesoro x then [x] else []
filtrarTesoros (x:xs) = if esTesoro x
                            then x : filtrarTesoros xs
                            else filtrarTesoros xs


--6. 
todosLosCaminos2 :: Mapa1 -> [[Dir]] 
--Devuelve todos lo caminos en el mapa.
todosLosCaminos2 (Cofre1 obj)             = []
--todosLosCaminos2 (Bifurcacion obj _ _) = [[obj]]
todosLosCaminos2 (Bifurcacion1 obj m1 m2) = (agregar Izq (todosLosCaminos2 m1)) ++ 
                                            (agregar Der (todosLosCaminos2 m2))


--Anexo con ejercicios adicionales
--4. Expresiones aritméticas
--Sea el tipo Exp, modelando expresiones aritméticas:
--data Exp = Constante Int
--          | ConsExpUnaria  OpUnaria  Exp
--          | ConsExpBinaria OpBinaria Exp Exp 
--data OpUnaria = Neg
--data OpBinaria = Suma | Resta | Mult | Div
---Implementar las siguientes funciones:
--1. eval :: Exp -> Int
--Dada una expresión evalúe esta expresión y retorne su valor.¿Qué casos hacen que eval sea una función parcial?
--eval (Constante x)             = x
--eval (ConsExpBinaria op e1 e2) = hacerOp op (eval e1) (eval e2)

--hacerOp :: OpBinaria -> Int -> Int -> Int
--hacerOp Suma n1 n2  = n1 + n2
--hacerOp Resta n1 n2 = n1 - n2
--hacerOp Mult n1 n2  = n1 * n2
--hacerOp Div n1 n2   = div n1 n2

--dividePorCero :: Exp -> Bool
--dividePorCero (Constante n) = False
--dividePorCero (ConsExpBinaria op e1 e2) = 
 --                                 chequearPorCero op e2 ||  dividePorCero e1 || dividePorCero e2

--chequearPorCero :: Exp -> Exp -> Bool
--chequearPorCero div (Constante 0 ) = True
--chequearPorCero _ _                = False


--2. simplificar :: Exp -> Exp
--Dada una expresión la simplifica según los siguientes criterios:
--a) 0+x=x+0=x b) x−0=x
--c) 0−x=−x
--d) x×1=1×x=x e) x×0=0×x=0 f) x÷1=x
--g) 0÷x=0, x̸=0

--simplificar (Constante x)             = Constante x
--simplificar (ConsExpBinaria op e1 e2) = simpOp op (simplificar e1) (simplificar e2)

--simpOp :: OpBinaria -> Exp -> Exp -> Exp
--simpOp Suma (Constante 0) x = x
--simpOp Suma x (Constante 0) = x
--simpOp Mult (Constante 1) x = x
--simpOp Mult x (Constante 1) = x
--simpOp Mult x (Constante 0) = x
--simpOp Mult (Constante 0) x = x
--simpOp op e1 e2 = ConsExpBinaria Mult e1 e2