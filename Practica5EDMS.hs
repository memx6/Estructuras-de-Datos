import Practica4Conjuntos
--MultiSet (MultiConjunto) 

--Ejercicio 5
--Un MultiSet (multiconjunto) es un tipo abstracto de datos similar a un Set (conjunto). A diferencia 
--del ultimo, cada elemento puede aparecer mas de una vez, y es posible saber la cantidad de ocurrencias 
--para un determinado elemento. Su interfaz es la siguiente:
data MultiSet a = MS [a]
     
emptyMS :: MultiSet a
--Crea un multiconjunto vacıo.
     
addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
     
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
     
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
--Dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
     
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
--Dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en comun.
     
multiSetToList :: MultiSet a -> [(Int,a)]
--Dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.

--1. Implementar el tipo abstracto MultiSet utilizando como representacion un Map. Indicar los ordenes de 
--complejidad en peor caso de cada funci ́on de la interfaz.

--2. Reimplementar como usuario de MultiSet la funcion ocurrencias de ejercicios anteriores, que dado un 
--string cuenta la cantidad de ocurrencias de cada caracter en el string. En este caso el resultado sera 
--un multiconjunto de caracteres.