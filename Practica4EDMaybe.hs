import Practica4Conjuntos

--8. Conjunto con Máximo (hacer anexo)
--Extender la interfaz de Set con la siguiente operación: 
--maximoC :: Ord a => Set a -> a
--Devuelve el máximo elemento en un conjunto
--1. Implementar la variante que recorre la estructura buscando el máximo 
--2. Implementar otra variante que no tenga que hacer un recorrido.

--------ANEXO------------
--1. Maybe
--1. Defina las siguientes operaciones (notar que son parciales si no devuelven Maybe):
data MaybeM a = JustM a
              | NothingM deriving (Show, Eq)
    
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show

--a)  
headM :: [a] -> MaybeM a -- Orden 1
--Versión de head que es total
headM [] = NothingM
headM xs = JustM (head xs)


--b) 
lastM :: [a] -> MaybeM a -- Orden N
--Dada una lista, devuelve su último elemento.
lastM [] = NothingM
lastM xs = JustM (last xs)


--c) 
maximumM :: Ord a => [a] -> MaybeM a
--Dada una lista de elementos devuelve el máximo.
maximumM [] = NothingM
maximumM xs = JustM (maximum xs)


--d) 
initM :: [a] -> MaybeM [a] -- Orden N
--Dada una lista quita su último elemento.
initM [] = NothingM
initM xs = JustM (init xs)


--e) 
get :: Int -> [a] -> MaybeM a -- Orden N
--Dado un índice devuelve el elemento de esa posición.
get _ [] = NothingM
get i xs = JustM (giveMe i xs)
    
giveMe :: Int -> [a] -> a --Orden N
giveMe i (x:xs) = if i == 0
                then x 
                else giveMe (i-1) xs


--f) 
indiceDe :: Eq a => a -> [a] -> MaybeM Int
--Dado un elemento y una lista devuelve la posición de la lista en la que se 
--encuentra dicho elemento.
indiceDe _ [] = NothingM
indiceDe e xs = JustM (giveMeInt e xs)

giveMeInt :: Eq a => a -> [a] -> Int --Orden N
giveMeInt e (x:xs) = if e == x
                then 0
                else 1 + giveMeInt e xs

--g) 
lookupM :: Eq k => [(k,v)] -> k -> MaybeM v --Orden n
--Dada una lista de pares (clave, valor) y una clave devuelve el valor asociado 
--a la clave.
lookupM xs k = JustM (giveValue xs k)

giveValue :: Eq k => [(k,v)] -> k -> v
giveValue (x:xs) k = if (fst x) == k
                            then snd x
                            else giveValue xs k

--h) 
fromJusts :: [MaybeM a] -> [a] --Orden n
--Devuelve los valores de los Maybe que no sean Nothing.
fromJusts [] = []
fromJusts (m:ms) = if isNothing m
                        then fromJusts ms
                        else (valueMaybe m) : fromJusts ms

isNothing :: MaybeM a -> Bool
isNothing NothingM = True
isNothing _        = False

valueMaybe :: MaybeM a -> a
valueMaybe (JustM x ) = x


--i) 
minT :: Ord a => Tree a -> MaybeM a 
--Dado un árbol devuelve su elemento mínimo.
minT EmptyT  = error "No tiene minimo" 
minT tree    = JustM (minimoT tree)

minimoT :: Tree a -> a
minimoT (NodeT x m1 m2) = min x (min (minimoT m1) (minimoT m2))



--2. Indicar los ordenes de complejidad en peor caso de cada función implementada.


-- remove :: a -> [a] -> [a]
-- remove elem (x:xs) = if elem == x
--                         then remove xs
--                         else x:(remove xs)


