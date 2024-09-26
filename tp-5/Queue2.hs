module Queue2

    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

    where 
    
    data Queue a = Queue [a]
    
    emptyQ :: Queue a                                   -- Costo constante
    emptyQ = Queue []

    isEmptyQ :: Queue a -> Bool
    isEmptyQ    (Queue xs) = null xs                    -- Costo constante

    enqueue :: a -> Queue a -> Queue a
    enqueue    x    (Queue xs) = (Queue (x:xs))    -- Costo lineal

    firstQ :: Queue a -> a
    firstQ  (Queue xs) = head xs                        -- Costo constante

    dequeue :: Queue a -> Queue a
    dequeue    (Queue xs) = if null xs
                                then emptyQ
                                else (Queue (sinElUltimo xs))           -- Costo lineal

    sinElUltimo :: [a] -> [a]
    sinElUltimo    []     = []
    sinElUltimo    (x:xs) = if null xs
                            then []
                            else x : sinElUltimo xs                     -- Costo lineal