import Termometro2

t = registrar 3 (registrar 2 (registrar 1 nuevoT))

corregir :: Int -> Term -> Term
corregir n t = nuevoCon (sumarN n (temperaturas t))

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = n + x : sumarN n xs