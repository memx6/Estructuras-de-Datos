--a) 
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
--Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas 
--en las que las personas programaron juntas.
programasEnComun p1 p2 org = if programaronJuntas org p1 p2
                                then intersection (programasDe org p1) (programasDe org p2)
                                else emptyS
--b) 
esUnGranHacker :: Organizador -> Persona -> Bool
--Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas 
--del organizador.
esUnGranHacker org p = nroProgramasDePersona org p == size(todosLosProgramas org)


data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
--a) Escribir los invariantes de representación para poder crear elementos válidos del TAD.
        --Inv Representacion:
        --- Map : No hay claves repetidas.
            -- Set: no hay elementos repetidos
            --balancear maps
            

--b) Implementar las funciones de la interfaz, respetando las restricciones de eficiencia pedidas. 
--Justifique en cada caso porqué se obtiene la eficiencia buscada.

nuevo :: Organizador
--Propósito: Un organizador vacío. Eficiencia: O(1)
nuevo = MkO EmptyM EmptyM
     
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
---Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto 
--de personas autores de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, 
--y el Set de personas no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (MkO(m1 m2)) c ps = MkO ((assocM c ps m1) (actualizarProgramas (set2list ps) c m2) )
     
actualizarProgramas :: [Persona] -> Checksum -> Map k v -> Map kv
actualizarProgramas [] c     m = m
actualizarProgramas [p:ps] c m = actualizarProgramas ps c (assocM p c )


todosLosProgramas :: Organizador -> [Checksum]
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del 
--organizador. Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MkO EmptyM _) = []
todosLosProgramas (MkO m1 m2)    = domM m1 
     

autoresDe :: Organizador -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado. Precondición: el 
--Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MkO m1 m2) c = maybeAValor(lookupM m1 c)

maybeAValor :: Maybe a -> a
maybeAValor (Just a) = a
     

--
programasDe :: Organizador -> Persona -> Set Checksum
--Propósito: denota el conjunto de programas en los que participó una determinada persona. 
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P ) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MkO m1 m2)  p = maybeAValor(lookupM p m2)
     

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores 
--de algún software en común. Precondición: las personas deben ser distintas.
--Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas 
--que aparecen en todos los programas del organizador, y C la cantidad total de programas.
programaronJuntas (MkO m1 m2) p1 p2 = not isEmptyS (intersectionS (programasDe p1) (programasDe p2))





--c) Implementar una variante del TAD Organizador suponiendo que en la interfaz del TAD 
--Organizador se agrega una nueva operación:
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) Maybe c
 --Inv Rep:
 ---Maybe c es el programa con mayor cantidad de autores
    --en caso de ser un organizador nuevo denota Nothing


elMayorPrograma :: Organizador -> Maybe Checksum
--Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador;
--denota Nothing si no puede devolver un programa.
--Eficiencia: O(1) en peor caso.
elMayorPrograma (MkO _ _ c) = c

--Esto puede requerir modificar el tipo de representación, agregar invariantes,
--y modificar operaciones existentes. Reescribir sólo las operaciones que tienen cambios sustanciales
--y no en las que, por ejemplo, sólo se modifica un pattern matching.

nuevo :: Organizador
--Propósito: Un organizador vacío. Eficiencia: O(1)
nuevo = MkO EmptyM EmptyM c