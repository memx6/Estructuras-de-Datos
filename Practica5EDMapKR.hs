--Ejercicio 2
--Implemente las distintas variantes del tipo Map vistas en clase:

--1. Como una lista de pares-clave valor con claves repetidas

data Map k v = M [(k, v)]

-- Inv. de Rep.
-- Hay claves repetidas

-- O(1)
emptyM :: Map k v
emptyM = M []

-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M xs) = buscar k xs

buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k [] = Nothing
buscar k (x:xs) = if k == fst x
                    then Just (snd x)
                    else buscar k xs

-- O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs) = M (agregar k v xs)

agregar :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
agregar k v xs = (k, v) : xs

-- O(n)
removeM :: Eq k => k -> Map k v -> Map k v
removeM k (M xs) = M (sacar k xs)

sacar :: Eq k => k -> [(k, v)] -> [(k, v)]
sacar k [] = []
sacar k (x:xs) = if k == fst x
                    then xs
                    else x : sacar k xs

-- O(n)
domM :: Eq k => Map k v -> [k]
domM (M xs) = claves xs

claves :: Eq k => [(k, v)] -> [k]
claves [] = []
claves (x:xs) = sinRepetidos(fst x : claves xs)

sinRepetidos :: Eq k => [k] -> [k]
sinRepetidos (x:xs) = if elem x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs