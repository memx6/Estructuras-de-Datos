module Termometro1(
	Term,
	nuevoT, 
	nuevoCon, 
	registrar, 
	temperaturas,
	cantidadT,
	maximaT,
 ) where

----------- Representacion
data Term = T [Int] deriving Show

-- Invariantes de Representación:
---- No tiene

nuevoT :: Term
nuevoT = T []

nuevoCon :: [Int] -> Term
nuevoCon xs = T xs

registrar :: Int -> Term -> Term
registrar x (T xs) = T (x:xs)

temperaturas :: Term -> [Int]
temperaturas (T xs) = xs

cantidadT :: Term -> Int
cantidadT (T xs) = length xs

maximaT :: Term -> Int
maximaT (T xs) = maximum xs
