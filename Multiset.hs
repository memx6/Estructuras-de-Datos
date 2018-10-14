data MultiSet = MkMS (Map Int (Set a)) (Set a)

-- Las claves del map representan la cantidad de apariciones
-- que tienen los elementos del set

-- El set de MkMS representa el conjunto de elementos
-- que más veces aparece en el multiset

-- Inv. Rep.
-- En el map no se guardan sets vacíos
-- Un elemento no puede aparecer en más de un set
-- En el set de MkMS aparecen los elementos con mayor clave del map

emptyMS :: MultiSet a -- O(1)
emptyMS = MkMS emptyM emptyS


appearsNTimes :: Ord a => Int -> MultiSet a -> [a]
--Devuelve los elementos que aparecen "n" veces
appearsNTimes n (MkMS m s) = maybeAValor(lookup n m):(appearsNTimes n (MkMS (remove n m)s))


maybeAValor :: Maybe Set -> Set a
maybeAValor Nothing = emptyS
maybeAValor (Just a) = a

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Dice la cantidad de veces que aparece un elemento.
ocurrencesMS a (MkMS m _) = (cantidadElem a (domM m) m)

cantidadElem :: a -> [k] -> Map k v -> Int
cantidadElem x [] m     = 0
cantidadElem x (c:cs) m = if belongs x (maybeAValor (lookupM c m))
                                then c
                                else cantidadElem a cs m

addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Aumenta en uno la cantidad de apariciones de un elemento.
addMS e ms = agregarYSacar e ms

agregarYSacar :: a -> MultiSet a -> MultiSet a
agregarYSacar e ms@(MkMS m s) = MkMS (borrarValor ms e (agregarValor e ms m) (compararMayor e s))

agregarValor :: a -> MultiSet a -> Map k v -> Map k v
agregarValor e ms m = assocM (1+ ocurrencesMS e ms) m

borrarValor :: MultiSet a -> a -> Map k v -> Map k v
borrarValor ms e m = removeM (ocurrencesMS e ms) m 

--2
appearances :: MultiSet a -> [(Int,[a])]
--Es una lista de tuplas, cuya primera componente es una cantidad 
--de apariciones y su segunda componente son los elementos que tienen dichas apariciones.
appearances (MkMS m _) = crearTuplas (domM m) m

crearTuplas:: [a] -> Map k v -> [(Int,[a])]
crearTuplas (x:xs) m = (x,(maybeAValor(lookupM x m))):crearTuplas xs m
    

maxMS :: MultiSet a -> Set a -- O(1)
maxMS (MkMS _ s) = s

-- COMO USUARIO:
mismasOcurrencias :: [a] -> MultiSet a -> Bool

maximaOcurrencia :: [a] -> Maybe a

ocurrenciasPar :: MultiSet a -> [(Int,a)]