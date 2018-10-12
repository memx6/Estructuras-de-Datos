data Tree = Empty | Node Int Tree Tree Tree

data Trie = MkT Tree

-- Inv. Rep.
-- Los Int de cada nodo >= 0
-- No hay hojas con Int == 0

vacioT :: Trie
vacioT = MkT Empty

ocurrencias :: String -> Trie -> Int
ocurrencias s (MkT t) = ocurrencias' s t

ocurrencias' _ Empty = 0
ocurrencias' [] (Node n t1 t2 t3) = n
ocurrencias' (x:xs) (Node n t1 t2 t3) = 
    case x of
       'a' -> ocurrencias' xs t1
       'b' -> ocurrencias' xs t2
       'c' -> ocurrencias' xs t3

agregarOcurrencia :: String -> Trie -> Trie
agregarOcurrencia s (MkT t) = MkT (agregarOc s t)

agregarOc [] Empty = Node 1 Empty Empty Empty
agregarOc (x:xs) Empty = 
    agregarOc (x:xs) (Node 0 Empty Empty Empty)
agregarOc [] (Node n t1 t2 t3) = 
    Node (n+1) t1 t2 t3
agregarOc (x:xs) (Node n t1 t2 t3) = 
    case x of
       'a' -> Node n (agregarOc t1) t2 t3
       'b' -> Node n t1 (agregarOc t2) t3
       'c' -> Node n t1 t2 (agregarOc t3)
         
borrarOcurrencia :: String -> Trie -> Trie
borrarOcurrencia s (MkT t) = MkT (borrarOc s t)

borrarOc :: String -> Tree -> Tree
borrarOc _ = Empty
borrarOc [] (Node n t1 t2 t3) = reconstruir (n-1) t1 t2 t3
borrarOc (x:xs) (NodeT n t1 t2 t3) =
    case x of
       'a' -> reconstruir n (borrarOc t1) t2 t3
       'b' -> reconstruir n t1 (borrarOc t2) t3
       'c' -> reconstruir n t1 t2 (borrarOc t3)

reconstruir 0 Empty Empty Empty = Empty
reconstruir n t1 t2 t3 = Node n t1 t2 t3

cantidadQueEmpiezanCon :: String -> Trie -> Int
cantidadQueEmpiezanCon s (MkT t) =
    cantQEmpCon s t
    
cantQEmpCon [] Empty = 0
cantQEmpCon [] (Node n t1 t2 t3) = 
    n + sumT t1 + sumT t2 + sumT t3
cantQEmpCon (x:xs) Empty = 0
cantQEmpCon (x:xs) (NodeT n t1 t2 t3) =
    case x of
       'a' -> cantQEmpCon xs t1
       'b' -> cantQEmpCon xs t2
       'c' -> cantQEmpCon xs t3  

vacioMS :: MultiSet
vacioMS = MS vacioT

esVacioMS (MS t) = cantidadQueEmpiezanCon "" t == 0

-- llamar a la interfaz del TAD

cantidadTotal :: Trie -> Int
cantidadTotal t = cantidadQueEmpiezanCon "" t

agregarPrefijos :: String -> Trie -> Trie
agregarPrefijos s t = agregarP (prefijos s) t

prefijos :: String -> [String]
prefijos [] = []
prefijos s  = s : prefijos (init s)

agregarP [] t = t
agregarP (p:ps) t = agregarOcurrencia p (agregarP ps t)

palabraConMasOcurrencias :: [String] -> Trie -> String
palabraConMasOcurrencias [x]    t = x
palabraConMasOcurrencias (x:xs) t = 
   let r = palabraConMasOcurrencias xs
       in if ocurrencias x t > ocurrencias r t
             then x
             else r