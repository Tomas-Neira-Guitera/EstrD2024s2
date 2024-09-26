module Stack 

    (Stack, emptyS, isEmptyS, push, top, pop, lenS)
    
    where

    data Stack a = Stack [a] Int
    -- Inv-Rep: El int representa la cantidad de elementos de la lista.

    emptyS :: Stack a
    emptyS = (Stack [] 0)

    isEmptyS :: Stack a -> Bool
    isEmptyS    (Stack xs int) = null xs

    push :: a -> Stack a -> Stack a
    push    x    (Stack xs int) = (Stack (x:xs) (1 + int))

    top :: Stack a -> a
    top    (Stack xs int) = head xs

    pop :: Stack a -> Stack a
    pop    (Stack xs int) = (Stack (tail xs) (int - 1))

    lenS :: Stack a -> Int
    lenS    (Stack xs int) = int