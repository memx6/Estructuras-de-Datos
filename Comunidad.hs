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
personas MkC (m1 m2 p) = domM m1

-- Propósito: devuelve el grupo al que pertenece una persona
-- O (log m)
gruposDe :: Persona -> Comunidad -> Set Grupo
gruposDe p (MkC m1 m2 p2) = maybeAValor(lookup p m1) 

-- Propósito: indica si la persona está en ese grupo
-- O ( log m + log s)
estaEnElGrupo :: Persona -> Grupo -> Comunidad -> Bool
estaEnElGrupo p gr (MkC _ m2 _) = belongs p (lookup gr m2)

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
agregar p gr (MkC m1 m2 mp) = MkC((assocM p gr m1) (actualizarGrupos (domM m2) p m2)

actualizarGrupos :: [a] -> Persona -> Map k v -> Map k v
actualizarGrupos (x:xs) p m = actualizarGrupos xs p (unionS (assocM p emptyM) (maybeAValor(lookupM x m)))

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

masGrande:: [a] -> Comunidad -> Maybe Grupo
masGrande [] com      = Nothing
masGrande [g] com     = Just g
masGrande (g:gps) com = if (sizeS(personasEn g com)) > ?????????????

-- Propósito: devuelve una lista de pares
-- donde la primera componente es el grupo en el que
-- aparece la persona (la segunda componente)
grupoPersona :: Comunidad -> [(Grupo, Persona)]
grupoPersona com = armarTuplas (personas com) com

armarTuplas :: [Personas] -> Comunidad -> [(Grupo, Persona)]
armarTuplas (p:ps) com = ((gruposDe p com), p):armarTuplas ps com

-- Propósito: junta dos comunidades
fusionar :: Comunidad -> Comunidad -> Comunidad
fusionar com1 com2 =  fusionComu (personas com1) (set2list(grupos com)) com2

fusionComu :: [Persona] -> [Grupo] -> Comunidad -> Comunidad
fusionComu (p:ps) (g:gps) com = fusionComu ps gps (agregar p g com)