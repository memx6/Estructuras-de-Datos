--Ejercicio 2
--Implemente las distintas variantes del tipo Map vistas en clase:

--2. Como una lista de paresclave valor sin claves repetidas
data Map k v = M [(k, v)]

-- Inv. de Rep.
-- No hay claves repetidas

-- O(1)
emptyM :: Map k v
emptyM = M []

-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M xs) = buscar k xs

buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k [] = Nothing
buscar k (x:xs) =
if k == fst x
   then Just (snd x)
   else buscar k xs

-- O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M xs) = agregar k v xs

agregar :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
agregar k v [] = [(k, v)]
agregar k v (x:xs) =
if k == fst x
   then (k, v) : xs
   else x : agregar k v xs

-- O(n)
removeM :: Eq k => k -> Map k v -> Map k v
removeM k (M xs) = sacar k xs

sacar :: Eq k => k -> [(k, v)] -> [(k, v)]
sacar k [] = []
sacar k (x:xs) =
if k == fst x
   then xs
   else x : sacar k xs

-- O(n)
domM :: Map k v -> [k]
domM (M xs) = claves xs

claves :: [(k, v)] -> [k]
claves [] = []
claves (x:xs) = fst x : claves xs