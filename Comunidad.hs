module Comunidad(Grupo, Persona, Comunidad) where

import Map
import Set

-- Una comunidad organiza personas en grupos

type Persona = String
type Grupo = String
data Comunidad = 
	MkC (Map Persona (Set Grupo)) (Map Grupo (Set Persona))
	    (Maybe Persona)
-- Invariantes de Representacion:
-- Sea MkC m1 m2 mb
-- No hay personas con grupos vacios
-- Los grupos y personas de m1 están en m2
-- mb es la persona con más grupos de m1
-- COMO IMPLEMENTADOR (calcular orden, cualquier orden)

-- Propósito: crea una comunidad vacía
--O (1)
crearComunidad :: Comunidad
crearComunidad = MkC emptyM emptyM Nothing

-- Propósito: devuelve todas las personas de la comunidad
-- O (n)
personas :: Comunidad -> [Persona]
personas (MkC m1 m2 p) = domM m1

-- Propósito: devuelve el grupo al que pertenece una persona
-- O (log m)
gruposDe :: Persona -> Comunidad -> Set Grupo
gruposDe p (MkC m1 m2 p2) = maybeAValor(lookup m1 p) 

-- Propósito: indica si la persona está en ese grupo
-- O ( log m + log s)
estaEnElGrupo :: Persona -> Grupo -> Comunidad -> Bool
estaEnElGrupo p gr (MkC _ m2 _) = belongs p (maybeAValor(lookup m2 gr))

-- Propósito: indica la persona que está en más grupos
-- O (1)
enMasGrupos :: Comunidad -> Maybe Persona
enMasGrupos (MkC _ _ mp) = mp

-- Propósito: indica las personas de un grupo
-- O (log m)
personasEn :: Grupo -> Comunidad -> Set Persona
personasEn gr (MkC m1 m2 mp) = maybeAValor(lookup gr m2)

-- Propósito: agrega una persona a un grupo
-- O ()
agregar :: Persona -> Grupo -> Comunidad -> Comunidad
agregar p gr (MkC m1 m2 mp) = MkC((actualizarPersonas p gr m1) (actualizarGrupos gr p m2)

actualizarGrupos :: Grupo -> Persona -> Map k v -> Map k v
actualizarGrupos g p m = assocM p (addS p (lookupM g m) m)

actualizarPersonas :: Persona -> Grupo -> Map k v -> Map k v
actualizarGrupos p g m = assocM g (addS g (lookupM p m) m)

-- COMO USUARIO (calcular orden, cualquier orden)
-- Propósito: indica los grupos de la comunidad
grupos :: Comunidad -> Set Grupo
grupos com = gruposDeComunidad (personas com) com

gruposDeComunidad :: [Personas] -> Comunidad -> Set Grupo
gruposDeComunidad (x:xs) com = unionS (gruposDe x com)(gruposDeComunidad xs com)

-- Propósito: indica si ambas personas comparten algun grupo
compartenAlgunGrupo :: Persona -> Persona -> Comunidad -> Bool
compartenAlgunGrupo p1 p2 com = not isEmptyS (intersection (gruposDe p1 com)(gruposDe p2 com)) 

-- Propósito: indica si la persona está en todos los grupos
enTodosLosGrupos :: Persona -> Comunidad -> Bool
enTodosLosGrupos p com = sizeS(gruposDe p com) == sizeS(grupos com)

-- Propósito: indica el grupo con más personas
grupoMasGrande :: Comunidad -> Maybe Grupo
grupoMasGrande com = masGrande (set2list (grupos com)) com

masGrande:: [Grupo] -> Comunidad -> Maybe Grupo
masGrande [] com      = Nothing
masGrande (g:gps) com = if (sizeS(personasEn g com)) > ?????


-- Propósito: devuelve una lista de pares
-- donde la primera componente es el grupo en el que
-- aparece la persona (la segunda componente)
grupoPersona :: Comunidad -> [(Grupo, Persona)]
grupoPersona com = armarTuplas (personas com) com

armarTuplas :: [Personas] -> Comunidad -> [(Grupo, Persona)]
armarTuplas [] com = []
armarTuplas (p:ps) com = (crearTupla(gruposDe p com))++ armarTuplas ps com

crearTupla :: [Grupo] -> Persona -> [(Grupo,Persona)]
crearTupla [] = []
crearTupla (g:gps) p = (g,p):crearTupla gps p
 
-- Propósito: junta dos comunidades
fusionar :: Comunidad -> Comunidad -> Comunidad
fusionar com1 com2 =  fusionComu (personas com1) (set2list(grupos com)) com2

fusionComu :: [Persona] -> [Grupo] -> Comunidad -> Comunidad
fusionComu [] [] com = com
fusionComu (p:ps) (g:gps) com = fusionComu ps gps (agregar p g com)



groupBy :: [Persona] -> Map Int [Persona]
groupBy []     = emptyM
groupBy (p:ps) = assocM (edad p ) (p : listarPorEdad (edad p)(groupBy ps)) groupBy ps

listarPorEdad :: Int -> Map Int [Persona] -> [Persona]
listarPorEdad n m = (dameValor (lookupM n m))

maxC :: Map String [Persona] -> Maybe Int
maxC m = Nothing
maxC m = maxEdad (domM m) m1

maxEdad :: [Int] -> Map Int [Persona] -> Maybe Int
maxEdad [] m     = Nothing
maxEdad (n:ns) m = maxM (sizeS(fromJust (lookupM n m))) (maxEdad ns m)

maxM :: Int -> Maybe Int -> Maybe Int
maxM n Nothing  = Just n 
maxM n (Just n') = Just (max n n')