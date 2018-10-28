data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector,Int)

tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
--O (s . log s ???)
tripulantes nave = getTripulantes (sectores nave) nave

getTripulantes:: [a] -> Nave -> Set Tripulante
getTripulantes (x:xs) nave = union (tripulantesDe x nave)(getTripulantes xs nave)


bajaDeTripulante :: Tripulante -> Nave -> Nave 
--Proposito: Elimina al tripulante de la nave.
--Pista: Considere reconstruir la nave sin ese tripulante.
-- O (S.P)
bajaDeTripulante t nave = reconstruir t n (naveVacia (sectores n))(sectores n)

--n = nave vieja
-- nv = nave nueva
reconstruir t n nv []     = n
reconstruir t n nv (s:ss) = agregar t s (tripulantesDe s n) (reconstruir t n nv ss)

agregar t s [] nv     = nv
agregar t s (t':ts) nv = if t == t' 
                            then agregar t s ts nv
                            else agregarTripulante t' s (agregar t s ts nv)



naveVacia :: [Sector] -> Nave                            
naveVacia xs = MkN ( consMap xs) emptyH (head xs , 0)

consMap :: [Sector] -> Map Sector (Set Tripulante)
consMap [] = emptyM
consMap (s:ss) = assocM s emptyS (consMap ss)


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


conRango :: Rango -> Nave -> Set Tripulante
--Proposito: Denota el conjunto de tripulantes con dicho rango. 
--Costo: O(P log P ) siendo P la cantidad de tripulantes.
conRango r (MkN _ h _) = igualRango r h


igualRango r h = if isEmptyH h
                    then emptyS
                    else agregar (findMin h) r (igualRango r (deleteMinH h))

agregar x r s = if (rango x) == r
                    then addS x s
                    else s


conMasTripulantes :: Nave -> Sector
--Proposito: Denota el sector de la nave con más tripulantes. 
--Costo: O(1).
conMasTripulantes (MkN _ _ s) = fst s


sectorDe :: Tripulante -> Nave -> Sector
--Proposito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S + log P ) siendo S la cantidad de sectores y P la cantidad de tripulantes.??
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
 --Costo: O (log S + log P).
agregarTripulante t s (MkN m h ms) = MkN((actualizarSectores s t m) (insertH t h) (compararMaximoSector s m ms))

actualizarSectores :: Sector -> Tripulante -> Map k v -> Map k v
 -- (log S + log P)
actualizarGrupos s t m = case lookup s m of
                                Just ts -> assocM s (addS t ts) m


compararMaximoSector :: Sector ->Map k v -> (Sector,Int)
compararMaximoSector s m (x,n)  = case lookup s m of    
                                            Just ts -> if (sizeS ts) +1 > n
                                                                then (s,sizeS ts +1)
                                                                else (x , n)


