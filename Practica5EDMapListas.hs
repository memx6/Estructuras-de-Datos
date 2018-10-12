--Ejercicio 2
--Implemente las distintas variantes del tipo Map vistas en clase:

--3. Como dos listas, una de claves y otra de valores, donde en la posicion i de 
--la lista de claves esta la clave del valor i de la lista de valores.

data Map k v = M [k][v] deriving (Show,Eq)

-- Inv. de Rep.
-- No claves repetidas
-- La longitud de k es igual a la longitud de v

-- O(1)
emptyM :: Map k v
emptyM = M [] []

-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M xs ys) = buscar k xs ys

buscar :: Eq k => k -> [k] -> [v] -> Maybe v
buscar k [] []= Nothing
buscar k (x:xs) ys = if k == x
                    then Just (head ys)
                    else buscar k xs (tail ys)

-- O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs ys) = if elem k xs
                            then (M xs (agregarEn (posicion k xs) v ys))
                            else (M (k:xs) (v:ys))

posicion :: Eq k => k -> [k] -> Int
posicion k (x:xs) = if (k == x)
                        then 0 
                        else 1 + (posicion k xs)


agregarEn :: Int -> v -> [v] -> [v]
agregarEn 0 v [] = []
agregarEn n v (x:xs) = if n == 0
                            then (v:xs)
                            else agregarEn (n-1) v xs

-- O(n)
--removeM :: Eq k => k -> Map k v -> Map k v
--removeM k (M xs ys) = M (sacar k xs ys)

--sacar :: Eq k => k -> [k] -> [v] -> [k] -> [v] 
--sacar k [] [] = [k]
--sacar k (x:xs) ys = if k == x
 --                   then xs (tail ys)
 --                   else x : sacar k xs (tail ys)

-- O(1)
domM :: Eq k => Map k v -> [k]
domM (M xs _) = xs
