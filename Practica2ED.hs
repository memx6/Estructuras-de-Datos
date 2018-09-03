--1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. 
--Luego implementar las siguientes funciones:

data Dir = Norte | Sur | Este | Oeste deriving Show
opuesto :: Dir -> Dir
--Dada una dirección devuelve su opuesta
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este


siguiente :: Dir -> Dir
--Dada una dirección devuelve su siguiente, en sentido horario.
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte


--2. Definir el tipo de dato Persona, como un nombre la edad de la persona. 
--Realizar las siguientes funciones:
data Persona = P String Int deriving Show

p1 = P "Mauro" 23
p2 = P "Fede" 25
nombre :: Persona -> String
nombre (P n e) = n
--Devuelve el nombre de una persona


edad :: Persona -> Int
edad (P n e) = e
--Devuelve la edad de una persona


crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)
--Dada una persona la devuelve con su edad aumentada en 1.


cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s (P n e) = P s e
--Dados un nombre y una persona, reemplaza el nombre de la persona por este otro.


esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra p1 p2 = (edad p1) < (edad p2)
--Dadas dos personas indica si la primera es más joven que la segunda.


mayoresA :: Int -> [Persona] -> [Persona]
--Dados una edad y una lista de personas devuelve todas las personas que son mayores a 
--esa edad.
mayoresA n [] = []
mayoresA n (x:xs) = mayorA n x ++ mayoresA n xs


mayorA :: Int -> Persona -> [Persona]
mayorA n p1 = if (edad p1 > n)
    then p1:[]
    else []


promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. La lista 
--al menos posee una persona.
promedioEdad [] = 0
promedioEdad xs = div (sumatoria xs) (length xs) 


sumatoria :: [Persona] -> Int
sumatoria [] = 0
sumatoria (x:xs) = edad x + sumatoria xs


--
elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. La lista al menos 
--posee una persona.
elMasViejo [] = error "No hay Personas en la lista"
elMasViejo [x] = x
elMasViejo (x:xs) = maxP x (elMasViejo xs)

maxP p1 p2 = if edad p1 > edad p2
                    then p1
                    else p2


--3. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y 
--un porcentaje de energía; y Entrenador, como un nombre y una lista de Pokemon. 
--Luego definir las siguientes funciones:
          
data TipoDePokemon = Agua | Fuego | Planta deriving Show
type Energia = Int
data Pokemon = Poke TipoDePokemon Energia deriving Show

type Nombre = String
data Entrenador = Entre Nombre [Pokemon] deriving Show

poke1 = Poke Agua 10
poke2 = Poke Fuego 20
poke3 = Poke Agua 40
poke4 = Poke Planta 50

en1 = Entre "Ash" [poke1,poke2,poke4]
en2 = Entre "Gary" [poke2,poke3]
en3 = Entre "pipo" [poke1,poke2,poke3]



energia :: Pokemon -> Int
energia (Poke t e) = e

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Agua Agua = True
mismoTipo Fuego Fuego = True
mismoTipo Planta Planta = True
mismoTipo _ _ = False 

leGana :: TipoDePokemon -> TipoDePokemon -> Bool
--Dado un TipoDePokemon y otro, devuelve True si el primero
--le gana al segundo. Agua gana a fuego, fuego a planta y planta a agua.
leGana Agua Fuego = True
leGana Fuego Planta = True
leGana Planta Agua = True
leGana Fuego Agua = False
leGana Planta Fuego = False
leGana Agua Planta = False

leGanaP :: Pokemon -> Pokemon -> Bool
--Dados dos pokemon indica si el primero le puede ganar al segundo. Se considera que 
--gana si su elemento es opuesto al del otro pokemon. Si poseen el mismo elemento 
--se considera que no gana.
leGanaP (Poke tpoke1 ex) (Poke tpoke2 ey) = if mismoTipo tpoke1 tpoke2
                                                then False
                                                else leGana tpoke1 tpoke2


capturarPokemon :: Pokemon -> Entrenador -> Entrenador
--Agrega un pokemon a la lista de pokemon del entrenador.
capturarPokemon poke (Entre n xs) = (Entre n (xs ++ [poke]))


cantidadDePokemons :: Entrenador -> Int
--Devuelve la cantidad de pokemons que posee el entrenador.
cantidadDePokemons (Entre n xs) = length xs


tipoPoke :: Pokemon -> TipoDePokemon
tipoPoke (Poke t e) = t

mismoTipoPorLista :: TipoDePokemon -> [Pokemon] -> Int
mismoTipoPorLista tipo [] = 0
mismoTipoPorLista tipo (x:xs) = if mismoTipo tipo (tipoPoke x)
                                    then 1 + mismoTipoPorLista tipo xs 
                                    else mismoTipoPorLista tipo xs


cantidadDePokemonsDeTipo :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de pokemons de determinado tipo que posee el entrenador.
cantidadDePokemonsDeTipo tipo (Entre n xs) = mismoTipoPorLista tipo xs

                           
tienePokemonGanador :: [Pokemon] -> Pokemon -> Bool
tienePokemonGanador [] poke = False
tienePokemonGanador (x:xs) poke = if leGanaP poke x
                                    then True
                                    else tienePokemonGanador xs poke
--
lePuedeGanar :: Entrenador -> Pokemon -> Bool
--Dados un entrenador y un pokemon devuelve True si el entrenador posee un pokemon que 
--le gane ese pokemon.
lePuedeGanar (Entre n xs) poke = tienePokemonGanador xs poke
                           
hayTipoAgua :: [Pokemon] -> Bool
hayTipoAgua [] = False
hayTipoAgua (x:xs) = if mismoTipo (tipoPoke x) Agua
                        then True
                        else hayTipoAgua xs


hayTipoFuego :: [Pokemon] -> Bool
hayTipoFuego [] = False
hayTipoFuego (x:xs) = if mismoTipo (tipoPoke x) Fuego
                            then True
                            else hayTipoFuego xs
                        
hayTipoPlanta :: [Pokemon] -> Bool
hayTipoPlanta [] = False
hayTipoPlanta (x:xs) = if mismoTipo (tipoPoke x) Planta
                            then True
                            else hayTipoPlanta xs
esExperto :: Entrenador -> Bool
--Dado un entrenador devuelve True si ese entrenador posee al menos un pokemon de cada 
--tipo posible.
esExperto (Entre n []) = False
esExperto (Entre n xs) = (hayTipoAgua xs && hayTipoFuego xs && hayTipoPlanta xs)
                           

pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (Entre n xs) = xs

fiestaPokemon :: [Entrenador] -> [Pokemon]
--Dada una lista de entrenadores devuelve una lista con todos los pokemon de cada 
--entrenador.
fiestaPokemon [] = []
fiestaPokemon (x:xs) = pokemonesDeEntrenador x ++ fiestaPokemon xs


--4. Tenemos los siguientes tipos de datos
data Pizza = Prepizza
     | Agregar Ingrediente Pizza deriving Show
data Ingrediente = Salsa
                   | Queso
                   | Jamon
                   | AceitunasVerdes Int deriving (Eq,Show)

pizza1 = Prepizza
pizza2 = Agregar Jamon(Agregar Jamon (Agregar Salsa pizza1 ))
pizza3 = Agregar (AceitunasVerdes 2) (Agregar Queso (Agregar Salsa pizza1 ) )
pizza4 = Agregar (AceitunasVerdes 5) (Agregar Queso (Agregar (AceitunasVerdes 3) pizza1 ) )
--Definir las siguientes funciones:


ingredientes :: Pizza -> [Ingrediente]
--Dada una pizza devuelve la lista de ingredientes
ingredientes Prepizza = []
ingredientes (Agregar ingre pizza) = ingre : ingredientes pizza


tieneJamon :: Pizza -> Bool
--Dice si una pizza tiene jamón
tieneJamon Prepizza = False
tieneJamon (Agregar ingre pizza) = ingre == Jamon || (tieneJamon pizza)
      
sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza      = Prepizza
sacarJamon (Agregar i p) =  if i == Jamon
                                then (sacarJamon p)
                                else (Agregar i (sacarJamon p))
      
armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza [] = Prepizza
armarPizza (i:is) = (Agregar i (armarPizza is))
    

esAceituna :: Ingrediente -> Bool
esAceituna (AceitunasVerdes _ )  = True
esAceituna _ = False

dobleAceitunas :: Ingrediente -> Ingrediente
-- Duplica la cantidad de aceitunas. Precondicion : Solo Aceitunas
dobleAceitunas (AceitunasVerdes x) = AceitunasVerdes (x*2)


duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza      = Prepizza
duplicarAceitunas (Agregar i p) =   if (esAceituna i)
                                        then (Agregar (dobleAceitunas i)(duplicarAceitunas p))
                                        else (Agregar i (duplicarAceitunas p))


sacar :: [Ingrediente] -> Pizza -> Pizza
--Saca los ingredientes de la pizza que se encuentren en la lista.
sacar [] pizza = pizza
sacar (i:is) pizza = sacarIngrediente i (sacar is pizza)

sacarIngrediente :: Ingrediente -> Pizza -> Pizza
sacarIngrediente ing Prepizza = Prepizza
sacarIngrediente ing (Agregar i p) = if (i == ing)
                                        then (sacarIngrediente ing p)
                                        else (Agregar i (sacarIngrediente ing p))
                    
 
contarJamon :: Pizza -> Int                               
contarJamon Prepizza = 0
contarJamon (Agregar i p) =  if i == Jamon
                                    then 1 + (contarJamon p)
                                    else 0 


cantJamon :: [Pizza] -> [(Int, Pizza)]
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad 
--de jamón de la pizza que es la segunda componente.
cantJamon [] = []
cantJamon (p:ps) = if tieneJamon p
                    then (((contarJamon p) , p) : (cantJamon ps))
                    else (0, p) : (cantJamon ps)

    
tieneAceitunas :: Pizza -> Bool
--Dice si una pizza tiene Aceitunas
tieneAceitunas Prepizza = False
tieneAceitunas (Agregar ingre pizza) = esAceituna ingre || (tieneAceitunas pizza)

cantAceitunas :: Pizza -> Int
-- Dada una pizza devuelve la cantidad de aceitunas que posee
cantAceitunas Prepizza = 0
cantAceitunas (Agregar i p) = if esAceituna i
                                    then (cantAceitunas1 i) + (cantAceitunas p)
                                    else cantAceitunas p

cantAceitunas1 :: Ingrediente -> Int
cantAceitunas1 (AceitunasVerdes x) = x
cantAceitunas1 _ = 0
                                     

mayorNAceitunas :: Int -> [Pizza] -> [Pizza]
--Devuelve las pizzas con más de “n” aceitunas.
mayorNAceitunas _ [] = []
mayorNAceitunas n (p:ps) = if ((cantAceitunas p) > n)
                                then p : (mayorNAceitunas n ps)
                                else (mayorNAceitunas n ps)


---------------------------------------------

--5. Tenemos los siguientes tipos de datos
data Objeto = Cacharro | Tesoro deriving (Eq,Show)
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving (Eq,Show)
--Definir las siguientes funciones:

objeto1 = Cacharro
objeto2 = Tesoro

c1 = Nada(Cofre[] Fin)
c2 = Cofre [objeto2] (Nada (Cofre [Cacharro] Fin))
c3 = Cofre [objeto2] (Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro]Fin))))

hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino.
hayTesoro Fin = False
hayTesoro (Cofre obj camino) = (hayTesoroEnLista obj) || hayTesoro camino
hayTesoro (Nada camino) = hayTesoro camino
 
esTesoro :: Objeto -> Bool
--Dado un objeto me Valida si es o no un Tesoro
esTesoro x = (x == Tesoro)

hayTesoroEnLista :: [Objeto] -> Bool
--Verifica que en la lista de objetos haya un Tesoro
hayTesoroEnLista [] = False
hayTesoroEnLista (ob:objs) = esTesoro ob || hayTesoroEnLista objs


pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. 
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Cofre objs cm) = if (hayTesoroEnLista objs) then 0 else 1 + pasosHastaTesoro cm
pasosHastaTesoro (Nada cm) = 1 + pasosHastaTesoro cm



hayTesoroEn :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, 
--indica si hay un tesoro en 5 pasos.
hayTesoroEn 0 camino = False
hayTesoroEn n camino = n == pasosHastaTesoro camino


alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros 0 camino = False
alMenosNTesoros n camino = (cantidadTesorosEn camino) >= n

cantidadTesorosEn :: Camino -> Int
cantidadTesorosEn Fin = 0
cantidadTesorosEn (Cofre objs camino) = if hayTesoroEnLista objs
                                then cantidadTesoros objs + (cantidadTesorosEn camino) 
                                else cantidadTesorosEn camino
cantidadTesorosEn (Nada camino) = 0 + (cantidadTesorosEn camino)                               

cantidadTesoros :: [Objeto] -> Int
cantidadTesoros [] = 0
cantidadTesoros (ob:objs) = if esTesoro ob  
                                then 1 + cantidadTesoros objs
                                else cantidadTesoros objs


cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, 
--si el rango es 3  5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. 
--Están incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre ri rf camino = (cantidadTesorosHasta rf camino) - (cantidadTesorosHasta ri camino)


cantidadTesorosHasta :: Int -> Camino -> Int
cantidadTesorosHasta n Fin = 0
cantidadTesorosHasta 0 (Cofre objs _) = cantidadTesoros objs
cantidadTesorosHasta n (Cofre objs camino) = cantidadTesoros objs + (cantidadTesorosHasta (n-1) camino) 
cantidadTesorosHasta n (Nada camino) = (cantidadTesorosHasta (n-1) camino)

------------------------------------------

--6. Tenemos los siguientes tipos de datos

data ListaNoVacia a = Unit a | Cons a (ListaNoVacia a) deriving Show


lista1 = (Unit 11 )
lista2 = (Cons 5 lista1 )
lista3 = (Cons 7 lista2)
--Definir las siguientes funciones:

length1 :: ListaNoVacia a -> Int
--Retorna la longitud de la lista.
length1 (Unit a) = 1
length1 (Cons a xs) = 1 + (length1 xs) 
       

head1 :: ListaNoVacia a -> a
--Devuelve el primer elemento de la lista. ¿Es una función parcial o total?
head1 (Unit a) = a
head1 (Cons a xs) = a 


tail2 :: ListaNoVacia a -> ListaNoVacia a
--Devuelve el resto de la lista sacando el primer elemento. Pensar bien qué pasa en caso de que 
--la lista tenga un sólo elemento. ¿Es una función parcial o total?
tail2 (Unit a) = Unit a
tail2 (Cons a xs) = xs 


minimo2 :: ListaNoVacia Integer -> Integer -- Me Rompe cuando utilizo Int
--Dada una lista retorna el mínimo de dicha lista. ¿Es una función parcial o total?
minimo2 (Unit a) = a
minimo2 (Cons a ls) = (min a (minimo2 ls))







                                    