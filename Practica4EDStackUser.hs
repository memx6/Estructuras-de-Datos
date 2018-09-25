import Practica4EDStack

--1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:
      
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show

apilar :: [a] -> Stack2 a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar [x:xs] = if isEmpty xs
                    then s
                    else push2 x s
         
isEmpty :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino 
--devuelve False.
isEmpty [] = True
isEmpty (x:xs) = if length xs >= 0
                        then False
                        else True
desapilar :: Stack2 a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar s = if isEmptyS2 s
                then []
                else top2 s : desapilar (pop2 s)         

treeToStack :: Tree a -> Stack2 a
--Dado un árbol devuelve una pila con los elementos apilados inorde.
treeToStack EmptyT = emptyS2
treeToStack (NodeT s s1 s2) = desapilar s



balanceado :: String -> Bool --(desafio)
--Toma un string que representa una expresión aritmética, por ejemplo ”(2 + 3) ∗ 2”, 
--y verifica que la cantidad de paréntesis que abren se corresponda con los que cierran. 
--Para hacer esto utilice una stack. Cada vez que encuentra un paréntesis que abre, 
--lo apila. Si encuentra un paréntesis que cierra desapila un elemento. 
--Si al terminar de recorrer el string se desapilaron tantos elementos como los que se 
--apilaron, ni más ni menos, entonces los paréntesis están balaceados. Pista: recorra
--una stack pasada por parámetro a una subtarea.
balanceado xs = chequear xs emptyS2

chequear :: String -> Stack2 Char -> Bool
chequear [] s = isEmptyS2 s
chequear (x:xs) s =
	if x == '('
	   then chequear xs (push2 '(' s)
	   else if x == ')' && not (isEmptyS2 s)
	   	       then chequear xs (pop2 s)
	   	       else False