--a) Implementar las siguientes funciones como usuario del TAD Nave, establecer su eficiencia y justificarla:

tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
--O (s . log s ???)
tripulantes nave = getTripulantes (sectores nave) nave

getTripulantes:: [a] -> Nave -> Set Tripulante
getTripulantes (x:xs) nave = union (tripulantesDe x nave)(getTripulantes xs nave)

--b) Opcional (Bonus): 
bajaDeTripulante :: Tripulante -> Nave -> Nave 
--Proposito: Elimina al tripulante de la nave.
--Pista: Considere reconstruir la nave sin ese tripulante.
bajaDeTripulante t nave@(MkN m h ms) = MkN(eliminarTripulante (domM m) t m nave)(actualizarH h t)(compararMaximoSector ms (sectorDe t nave)nave)

eliminarTripulante :: [Sector] -> Tripulante -> Map k v -> Nave -> Map k v
eliminarTripulante (x:xs) t m nave = if (sectorDe t nave) == x
                                        then removeM m x
                                        else eliminarTripulante xs t m nave

actualizarH:: Heap -> Tripulante -> Heap
actualizarH h t = if (findMin h) == t
                    then deleteMin h
                    else ????????????

--b) Implementar el TAD Nave suponiendo el siguiente tipo de representación:

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int) 
--Donde:
--Cada tripulante puede estar en un sector como máximo.
--Se guarda al sector con más tripulantes de la nave y cuántos tripulantes tiene ese sector.
--Los tripulantes se ordenan por rango de mayor a menor en la Heap (no se confunda, findMin devuelve 
--al tripulante con mayor rango).
--a) Escribir los invariantes de representación para poder crear elementos válidos del TAD.
 --Inv Rep 
 ---No hay tripulantes repetidos entre los sectores
 --- el Set de tripulantes y la heap de Tripulantes tienen la misma longitud.
 --- No hay una nave sin sectores, el map no esta vacio

--b) Implementar las funciones de la interfaz, calculando eficiencia. Justifique en cada caso la eficiencia
 --obtenida.

 naveVacia :: [Sector] -> Nave
 --Propósito: Crea una nave con todos esos sectores sin tripulantes. 
 --Precondición: la lista de sectores no está vacía
 --Costo: O(S log S) siendo S la cantidad de sectores de la lista.
 naveVacia (xs) = MkN ( agregarSectores xs emptyM ) emptyH ((head xs),0)
      
 agregarSectores :: [Sector] -> Map k v -> Map k v
 -- O ( log n)
 agregarSectores (x:xs) m = agregarSectores xs (assocM x emptyS m)


 tripulantesDe :: Sector -> Nave -> Set Tripulante
 --Proposito: Obtiene los tripulantes de un sector. 
 --Costo: O(log S) siendo S la cantidad de sectores.
 tripulantesDe s (MkN m _ _) = dameValor(lookup m s) 
 
 dameValor :: Maybe Set a-> Set a
 dameValor Nothing  = emptyS
 dameValor (Just a) = a
      
 
 sectores :: Nave -> [Sector]
 --Proposito: Denota los sectores de la nave 
 --Costo: O(S) siendo S la cantidad de sectores.
 sectores (MkN m _ _) = domM m
      
 
 conMayorRango :: Nave -> Tripulante
 --Proposito: Denota el tripulante con mayor rango. 
 --Precondicion: la nave no está vacía.
 --Costo: O(1).
 conMayorRango (MkN _ h _) = findMin h
      
 
 conMasTripulantes :: Nave -> Sector
 --Proposito: Denota el sector de la nave con más tripulantes. 
 --Costo: O(1).
 conMasTripulantes (MkN _ _ s) = fst s
      
 
 conRango :: Rango -> Nave -> Set Tripulante
 --Proposito: Denota el conjunto de tripulantes con dicho rango. 
 --Costo: O(P log P ) siendo P la cantidad de tripulantes.
 conRango r nave = igualRango r (tripulantesDeNave (sector nave) nave)

tripulantesDeNave :: [Sector] -> Nave -> [Tripulante]
-- O (S log S) siendo S la cantidad de setores.
tripulantesDeNave (x:xs) n = (set2list(tripulantesDe x n)) ++ (tripulantesDeNave xs n )

igualRango:: Rango -> [Tripulante] -> Set Tripulante
--O (T log T) siendo T la cantidad de tripulantes, 
igualRango r (t:ts) = if r == (rango t)
                            then (addS t emptyS) ++ igualRango r ts
                            else igualRango r ts

 
sectorDe :: Tripulante -> Nave -> Sector
 --Proposito: Devuelve el sector en el que se encuentra un tripulante.
 --Precondición: el tripulante pertenece a la nave.
 --Costo: O(S log S log P ) siendo S la cantidad de sectores y P la cantidad de tripulantes.??
sectorDe t nave@(MkN m _ _) = dameSector t (sectores nave) m

dameSector :: Tripulante -> [Sector] -> Map k v -> Sector
-- O (S Log S + log P)
dameSector t [] m     = error 
dameSector t (x:xs) m = case lookup x m of
                                Just ts -> if belongs ts
                                                then x
                                                else dameSector t xs m
      
 
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
 --Proposito: Agrega un tripulante a ese sector de la nave. 
 --Precondicion: El sector está en la nave y el tripulante no. 
 --Costo: No hay datos (justifique su elección).
agregarTripulante t s nave@(MkN m h ms) = MkN((actualizarSectores (domM m) t m) (insertH t h) (compararMaximoSector ms s nave))

actualizarSectores :: [Sector] -> Tripulante -> Map k v -> Map k v
 -- (S log S)
actualizarGrupos (x:xs) t m = actualizarSectores xs t (unionS (assocM t emptyM) (maybeAValor(lookupM x m)))

compararMaximoSector :: (Sector,Int) -> Sector -> Nave -> (Sector,Int)
compararMaximoSector (x,y) s n = if sizeS(tripulantesDe s n) > y
                                        then (s,tripulantesDe s n)
                                        else (x,y)

 