module Queue

    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

    where

    data Queue a = Queue [a]

    emptyQ :: Queue a 
    emptyQ = (Queue [])

    isEmptyQ :: Queue a -> Bool
    isEmptyQ    (Queue xs) = null xs

    enqueue :: a -> Queue a -> Queue a 
    enqueue    x    (Queue xs) = (Queue (agregarAlFinal x xs))

    agregarAlFinal :: a -> [a] -> [a]
    agregarAlFinal    x    []  = [x]
    agregarAlFinal    x    (y:ys) = y : (agregarAlFinal x ys)

    firstQ :: Queue a -> a 
    firstQ    (Queue xs) = head xs

    dequeue :: Queue a -> Queue a 
    dequeue    (Queue xs) = (Queue (tail xs))