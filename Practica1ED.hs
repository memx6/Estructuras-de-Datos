--c) 
maximo :: Int -> Int -> Int
--Dados dos números devuelve el mayor de estos.
maximo x y = 
        if x > y
                then x
                else y



suma2 :: Int -> Int
suma2 x = x + 2


sumatoria x = 
        if x > 0
            then x + sumatoria (x - 1)
            else 0



--a) 
sucesor :: Int -> Int
--Dado un número devuelve su sucesor
sucesor x = x + 1

--b) 
sumar :: Int -> Int -> Int
--Dados dos números devuelve su suma utilizando la operación +.
sumar x y = x + y

--c) 
maxim :: Int -> Int -> Int
--Dados dos números devuelve el mayor de estos.
maxim x y = 
        if x > y
                then x
                else y


--2. Defina las siguientes funciones utilizando pattern matching:
--a) 
negar :: Bool -> Bool
--Dado un booleano, si es True devuelve False, y si es False devuelve 
--True. Definida en Haskell como not.
negar True = False
negar False = True

--b) 
andLogico :: Bool -> Bool -> Bool
--Dados dos booleanos si ambos son True devuelve True, sino devuelve 
--False. Definida en Haskell como &&.
andLogico True True = True
andLogico False True = False
andLogico True False = False
andLogico False False = False


--c) 
orLogico :: Bool -> Bool -> Bool
--Dados dos booleanos si alguno de ellos es True devuelve True, sino 
--devuelve False. Definida en Haskell como ||.
orLogico True True = True
orLogico False True = True
orLogico True False = True
orLogico False False = False


--d) 
primera :: (Int,Int) -> Int
--Dado un par de números devuelve la primera componente. Definida en 
--Haskell como fst.
primera (x,y) = x

--e) 
segunda :: (Int,Int) -> Int
--Dado un par de números devuelve la segunda componente. Definida en Haskell como snd.
segunda (x,y) = y

--f) 
sumaPar :: (Int,Int) -> Int
--Dado un par de números devuelve su suma.
sumaPar (x, y) = sumar x y 
--g) 
maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (x, y) = maxim x y


--3. Defina las siguientes funciones polimórficas:
--a) 
loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo x = x


--b) 
siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete x = 7


--c) 
duplicar :: a -> (a,a)
--Dado un elemento de algún tipo devuelve un par con ese elemento en 
--ambas componentes.
duplicar x = (x, x)


--d) 
singleton :: a -> [a]
---Dado un elemento de algún tipo devuelve una lista con este único 
--elemento.
singleton x = [x]


--4. Defina las siguientes funciones polimórficas utilizando 
--pattern matching sobre listas:

--a) 
isEmpty :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino 
--devuelve False.
isEmpty [] = True
isEmpty (x:xs) = if length xs >= 0
                        then False
                        else True


--b) 
head' :: [a] -> a
--Dada una lista devuelve su primer elemento.
head' [] = error "lista Vacia"
head' (x:xs) = x


--c) 
tail' :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
tail' [] = error "Lista Vacia"
tail' (x:xs) = xs



--Practica Recursion
-- 2.1 Recursion sobre listas
-- 1. sumatoria :: [Int] -> Int
-- Dada una lista de enteros devuelve la suma de todos sus elementos.

sumatorial :: [Int] -> Int
sumatorial [] = 0
sumatorial (x:xs) = x + sumatorial xs

-- 2. 
longitud :: [a] -> Int
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, 
-- es decir, la cantidad de elementos que posee.
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


-- 3. 
mapSucesor :: [Int] -> [Int]
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
mapSucesor [] = []
mapSucesor (x:xs) = 
        (x + 1) : mapSucesor xs

-- 4. 
mapSumaPar :: [(Int,Int)] -> [Int]
-- Dada una lista de pares de enteros, devuelve una nueva lista en la que cada 
-- elemento es la suma de los elementos de cada par.
mapSumaPar [] = []
mapSumaPar ((x,y):xs) = [(x + y)] ++  mapSumaPar xs

-- 5. 
mapMaxDelPar :: [(Int,Int)] -> [Int]
-- Dada una lista de pares, devuelve una nueva lista en la que cada elemento es 
--el mayor de las componentes de cada par.
mapMaxDelPar [] = []
mapMaxDelPar ((x,y):xs) = [max x y ] ++ mapMaxDelPar xs


--6. 
todoVerdad :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si todos sus elementos son True.
todoVerdad [] = True
todoVerdad (x:xs) = x && todoVerdad xs
               

--7. 
algunaVerdad :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
algunaVerdad [] = False
algunaVerdad (x:xs) = x || algunaVerdad xs
 
-- 8. 
pertenece :: Eq a => a -> [a] -> Bool
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs 
-- que sea igual a e.
pertenece y [] = False
pertenece y (x:xs)= y == x || pertenece y xs


--9. 
apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones n [] = 0
apariciones n (x:xs) = 
                if n == x
                        then 1 + apariciones n xs
                        else 0 + apariciones n xs

--10. f
filtrarMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son 
--menores a n.
filtrarMenoresA n [] = []
filtrarMenoresA n (x:xs) =
        if x < n
                then x : filtrarMenoresA n xs
                else filtrarMenoresA n xs


--11. 
filtrarElemento :: Eq a => a -> [a] -> [a]
--Dados un elemento y una lista filtra (elimina) todas las ocurrencias de ese 
--elemento en la lista.
filtrarElemento n [] = []
filtrarElemento n (x:xs) =
        if x == n
                then x : filtrarElemento n xs
                else filtrarElemento n xs


--12. 
mapLongitudes :: [[a]] -> [Int]
--Dada una lista de listas, devuelve la lista de sus longitudes. Aplique esta 
--función a la lista de strings ["Estructuras", "de", "datos"] y observe el 
--resultado.
mapLongitudes [] = []
mapLongitudes (xs:xss) =
        length xs : mapLongitudes xss


--13. 
longitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas 
--que tienen más de n elementos.
longitudMayorA n [] = []
longitudMayorA n (xs:xss) =
        if length xs > n
                then xs : longitudMayorA n xss
                else longitudMayorA n xss


--14. 
intercalar :: a -> [a] -> [a]
--Dado un elemento e y una lista xs, ubica a e entre medio de todos los elementos 
--de xs. Ejemplo:
--   intercalar ’,’ "abcde" == "a,b,c,d,e"
intercalar e [] = []
intercalar e (x:[]) = x:[]
intercalar e (x:xs) = 
        x : e : intercalar e xs  

-- 15. 
snoc :: [a] -> a -> [a]
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado 
-- al final de la lista.
snoc [] y = [y]
snoc (x:xs) y = x : (snoc xs y)


-- 16. 
append :: [a] -> [a] -> [a]
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista 
-- y todos los elementos de la segunda a continuación. Definida en Haskell como ++.
append [] ys = ys
append (x:xs) ys =
        x : append xs ys


--17. 
aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss


-- 18. 
reversa :: [a] -> [a]
-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. 
-- Definida en Haskell como reverse.
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


--19. 
--zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición
 --n es el máximo entre el elemento n de la primera lista y de la segunda lista, 
 --teniendo en cuenta que las listas no necesariamente tienen la misma longitud.
 --zipMaximos [][] = []
 --zipMaximos (x:xs) (y:ys) = [(maxim x y)] ++ zipMaximos xs ys


-- 20. 
--zipSort :: [Int] -> [Int] -> [(Int,Int)]
-- Dadas dos listas de enteros de igual longitud, devuelve una lista de pares 
-- (min, max), donde min y max son el mínimo y el máximo entre los elementos de 
-- ambas listas en la misma posición.

--zipSort [x][y] = [(x,y)]
--zipSort (x:xs)(y:ys) = if length xs == length ys
--                        then ( (minimo (x y)) ,(maxim (x y)) ) : (zipSort xs ys)
 --                       else error "Listas de diferente longitud"

--21. 
promedio :: [Int] -> Int
--Dada una lista de enteros, devuelve un número que es el promedio entre todos 
--los elementos de la lista. ¿Pudo resolverla utilizando recursión estructural?
promedio [] = 0
promedio xs = div (sumatorial xs) (length xs)

-- 22. 
minimo :: Ord a => [a] -> a --minumum: puse otro nombre al del enunciado 
--ya que me daba ambiguedad de funciones
-- Dada una lista devuelve el mínimo
minimo [] = error "Minimo de una lista vacía"
minimo (x:[]) = x
minimo (x:xs) = if x < minimo xs then x else minimo xs



--2.2.Recursión sobre números
--Defina las siguientes funciones utilizando recursión sobre números enteros, 
--salvo que se indique lo contrario:

--1. 
factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores 
--hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)


-- 2. 
cuentaRegresiva :: Int -> [Int]
-- Dado un número n devuelve una lista cuyos elementos sean los números 
-- comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la 
-- lista vacía.
cuentaRegresiva n = if n < 1
                        then []
                        else [n] ++ cuentaRegresiva (n-1)


--3. 
contarHasta :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números entre 1 y n 
--(incluidos).
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]


--4. 
replicarN :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
replicarN 0 x = []
replicarN n x = x : replicarN (n-1) x


--5. 
--desdeHasta :: Int -> Int -> [Int]
--Dados dos números n y m devuelve una lista cuyos elementos sean los números entre n 
--y m (incluidos).
--desdeHasta 0 0 = []
--desdeHasta n m = if n /= m
--                then desdeHasta n (m-1) ++ [n]
--                else desdeHasta n m ++ [m]

 

-- 6. 
takeN :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista con los primeros n 
-- elementos de xs. Si xs posee menos de n elementos, se devuelve la lista completa.
takeN 0 xs      = []
takeN n (x:xs) 
                | n > 0 = x : takeN (n-1) xs
                | otherwise = takeN n xs

--7. 
dropN :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos 
--de lista recibida. Si la lista posee menos de n elementos, se devuelve una lista vacía.
dropN 0 xs = xs 
dropN n [] = []  
dropN n (x:xs) = dropN (n-1)  xs 


-- 8. 
splitN :: Int -> [a] -> ([a], [a])
-- Dados un número n y una lista xs, devuelve un par donde la primera componente 
-- es la lista que resulta de aplicar takeN a xs, y la segunda componente el resultado 
-- de aplicar dropN a xs. ¿Conviene utilizar recursión?
splitN n xs = (takeN n xs , dropN n xs)

--splitMin :: Ord a => [a] => (a,[a])
--splitMin [] = error "no tiene minimo"
--splitMin [x] = (x,[])
---splitMin (x:xs) = agregarMin x (splitMin xs)

--agregarMin x (m, xssm) =
  --      if x < m 
   --             then (x, m : xssm)
   --             else (m, x : xssm)














