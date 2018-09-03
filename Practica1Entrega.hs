
-- 3. 
mapSucesor :: [Int] -> [Int]
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
mapSucesor [] = []
mapSucesor (x:xs) = 
        (x + 1) : mapSucesor xs

-- 8. 
pertenece :: Eq a => a -> [a] -> Bool
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs 
-- que sea igual a e.
pertenece _ [] = False
pertenece y (x:xs)
    | y == x       = True
    | otherwise = pertenece y xs

-- 11. 
filtrarElemento :: Eq a => a -> [a] -> [a]
-- Dados un elemento y una lista filtra (elimina) todas las ocurrencias de ese 
-- elemento en la lista.
filtrarElemento _ [] = []
filtrarElemento y (x:xs)
    | y /= x       = x : filtrarElemento y xs
    | otherwise = filtrarElemento y xs


-- 15. 
snoc :: [a] -> a -> [a]
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado 
-- al final de la lista.
snoc [] _ = []
snoc [x] y = [x,y]
snoc (x:xs) y = [x] ++ snoc xs y


-- 16. 
append :: [a] -> [a] -> [a]
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista 
-- y todos los elementos de la segunda a continuación. Definida en Haskell como ++.
append [] ys = ys
append (x:xs) ys =
        x : append xs ys

-- 18. 
reversa :: [a] -> [a]
-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. 
-- Definida en Haskell como reverse.
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


-- 22. 
minimo :: Ord a => [a] -> a --minumum: puse otro nombre al del enunciado 
--ya que me daba ambiguedad de funciones
-- Dada una lista devuelve el mínimo
minimo [] = error "Minimo de una lista vacía"
minimo (x:[]) = x
minimo (x:xs) = if x < minimo xs then x else minimo xs


-- 2. 
cuentaRegresiva :: Int -> [Int]
-- Dado un número n devuelve una lista cuyos elementos sean los números 
-- comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la 
-- lista vacía.
cuentaRegresiva n = if n < 1
                        then []
                        else [n] ++ cuentaRegresiva (n-1) 


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


splitNBis :: Int -> [a] -> ([a], [a])
splitNBis 0 xs = ([], xs)
splitNBis n [] = ([],[])
splitNBis n (x:xs) = 
                agregar x (splitNBis (n-1)xs)

agregar x (xs,ys) = (x:xs, ys)               


--------------- fuera de la entrega ------------


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