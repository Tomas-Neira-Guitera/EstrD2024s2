{-

head' :: [a] -> a
head' (x:xs) = x            -- O(1) 

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 -- O(1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)   -- O(n) donde n es = al int que recibe la funcion

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs   -- O(n) donde n es la longitud de la lista

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs       -- O(n*m) donde n es la longitud de la lista y m es el numero mas grande dentro de la lista.

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs       -- O(n) donde n es la longitud de la lista

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs        -- O(n * (n-1)) donde n es la longitud de la lista.

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys                     -- O(n) donde n es la longitud de la lista que se recorre.

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs                  -- O(n*m) donde n es la longitud de la lista que se recorre y m es la palabra con mas letras dentro de la lista n.

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs                     -- O(n) donde n es el min entre el Int y la cantidad de elementos de la lista. 

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs                         -- O(n) donde n es el min entre el Int y la cantidad de elementos de la lista.

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)                  -- O(n) donde n es el min entre el Int y la cantidad de elementos de la lista.

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)                       -- O(n) donde n es la cantidad de elementos de la lista.

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs                 -- O(n) donde n es la cantidad de elementos de la lista.

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
            let m = minimo xs
            in m : ordenar (sacar m xs)                 -- O(n) donde n es la cantidad de elementos de la lista.

-}
-------------------------------------------------------------------------------------
{-

import Set

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen            []     _     = []
losQuePertenecen            (x:xs) set   = if belongs x set
                                            then x : (losQuePertenecen xs set)
                                            else losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos            xs  = setToList (listToSet xs)

listToSet :: Eq a => [a] -> Set a 
listToSet            []     = emptyS
listToSet            (x:xs) = addS x (listToSet xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos            EmptyT          = emptyS
unirTodos            (NodeT x ti td) = unionS x (unionS (unirTodos ti) (unirTodos td))

-}
-------------------------------------------------------------------------------------
{-

import Queue

lengthQ :: Queue a -> Int
lengthQ    queue   = if isEmptyQ queue
                        then 0
                        else 1 + (lengthQ (dequeue queue))

queueToList :: Queue a -> [a]
queueToList    queue   = if isEmptyQ queue
                            then []
                            else (firstQ queue) : (queueToList (dequeue queue))

unionQ :: Queue a -> Queue a -> Queue a
unionQ    queue      queue2  = if isEmptyQ queue
                                then queue2
                                else enqueue (firstQ queue) (unionQ (dequeue queue) queue2)

-}
-------------------------------------------------------------------------------------
import Stack -- (Stack, emptyS, isEmptyS, push, top, pop, lenS)

-- Proposito: Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar    []     = emptyS
apilar    (x:xs) = push x (apilar xs)


-- Proposito: Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar    stack   = if isEmptyS stack
                        then [] 
                        else top stack : (desapilar (pop stack))


-- Proposito: Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos n x s = apilar (insertarElemEn n x (desapilar s))


insertarElemEn :: Int -> a -> [a] -> [a]
insertarElemEn _ a []       = [a]
insertarElemEn 0 a xs       = a : xs
insertarElemEn n a (x : xs) = x : (insertarElemEn (n-1) a xs)