module Queue3

    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

    where 
    
    data Queue a = Queue [a] [a]
    --                   fs   bs

    -- INV-REP: Si fs se encuentra vacía, entonces la cola se encuentra vacía.

    
    emptyQ :: Queue a                                   -- Costo constante
    emptyQ = Queue [] []

    isEmptyQ :: Queue a -> Bool
    isEmptyQ    (Queue fs bs) = null fs                    -- Costo constante

    enqueue :: a -> Queue a -> Queue a
    enqueue    x    (Queue fs bs) = if null fs 
                                        then Queue (x:fs) bs
                                        else Queue  fs (x:bs)       -- Costo constante  

    firstQ :: Queue a -> a
    firstQ  (Queue fs bs) = head fs                        -- Costo constante

    dequeue :: Queue a -> Queue a
    dequeue    (Queue fs bs) = if null (tail fs)
                                then Queue (reverse bs) []
                                else Queue (tail fs) bs