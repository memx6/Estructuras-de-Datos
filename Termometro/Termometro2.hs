module Termometro2(
	Term,
	nuevoT, 
	nuevoCon, 
	registrar, 
	temperaturas,
	cantidadT,
	maximaT,
 ) where

----------- Representacion
data Term = T [Int] Int Int deriving Show

-- Invariantes de Representación:
---- Sea (T xs n m) un termometro
------- * n es la longitud de xs
------- * m es el maximo de xs

nuevoT :: Term
nuevoT = T [] 0 0

nuevoCon :: [Int] -> Term
nuevoCon xs = T xs (length xs) (maximum xs)

registrar :: Int -> Term -> Term
registrar x (T xs n m) = T (x:xs) (n+1) (max x m)

temperaturas :: Term -> [Int]
temperaturas (T xs _ _) = xs

cantidadT :: Term -> Int
cantidadT (T xs n m) = n

maximaT :: Term -> Int
maximaT (T xs n m) = m
