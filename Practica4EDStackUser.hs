import Practica4EDStack

--1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:
      
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show

apilar :: Ord a => [a] -> Stack2 a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar [] = emptyS2
apilar xs = agregarElementos xs emptyS2

agregarElementos :: Ord a => [a] -> Stack2 a -> Stack2 a
agregarElementos [] s     = s
agregarElementos (x:xs) s = agregarElementos xs (push2 x s)
         
isEmpty :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino 
--devuelve False.
isEmpty [] = True
isEmpty xs = (length xs) > 0


desapilar :: Stack2 a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar s = if isEmptyS2 s
                then []
                else top2 s : desapilar (pop2 s)         

treeToStack :: Tree a -> Stack2 a
--Dado un árbol devuelve una pila con los elementos apilados inorder.
treeToStack EmptyT = emptyS2
treeToStack tree   = agregarElementos (elemInOrder tree) emptyS2


elemInOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo 
--inorder. Nota: En el modo inorder primero se procesan los elementos del hijo 
--izquierdo, luego la raiz y luego los elementos del hijo derecho.
elemInOrder EmptyT          = []
elemInOrder (NodeT a t1 t2) = (elemInOrder t1) ++ [a] ++ (elemInOrder t2)

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
chequear (x:xs) s = if x == '('
    then chequear xs (push2 '(' s)
    else if x == ')' && not (isEmptyS2 s)
        then chequear xs (pop2 s)
        else False