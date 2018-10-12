
type Software = String
data Autor = Admin String 
            | Dev 
data Organizador = Agregar Software [Autor] Organizador 
                  | Vacio

pares :: Organizador -> [(Software,Int)]
pares Vacio = []
pares (Agregar s xs os) = (s, length ) : pares os

enComun :: Autor -> Autor -> Organizador -> Organizador
enComun a1 a2 Vacio = 
enComun a1 a2 (Agregar s xs or) = if esta a1 xs && esta a2 xs       
                                        then s: enComun a1 a2 or    
                                        else enComun a1 a2 or
                    
filtrar :: [Autor] -> Organizador -> Organizador
filtrar ys Vacio = Vacio
filtrar ys (Agregar s xs or) = Agregar s (quitar ys xs) (filtrar ys or)

quitar :: [Autor] -> [Autor]-> [Autor]
quitar ys [] = []
quitar ys (x:xs) = if elem x ys     
                        then quitar ys xs
                        else x: quitar ys xs
            
losAdmin :: Organizador -> [Autor]
losAdmin Vacio = []
losAdmin (Agregar s xs os) = (admins xs):(losAdmin or)


ordenados :: Organizador -> [Software]
ordenados Vacio = []
ordenados (Agregar s xs or) = ordenar (pares or)

ordenar :: [(Software,Int)] -> [(Software,Int)]
ordenar [] = []
ordenar xs = minimo xs : (sacar (minimo xs) xs))