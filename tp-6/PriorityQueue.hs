module PriorityQueue

    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

    where

    data PriorityQueue a = PriorityQueue [a]

    emptyPQ :: PriorityQueue a
    emptyPQ =  (PriorityQueue [])

    isEmptyPQ :: PriorityQueue a -> Bool
    isEmptyPQ    (PriorityQueue xs) = null xs

    insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
    insertPQ             x    (PriorityQueue xs) = (PriorityQueue (insertPQL x xs))

    insertPQL :: Ord a => a -> [a] -> [a]
    insertPQL             x    []       = [x]
    insertPQL             x    (y : ys) = if x < y 
                                            then x : y : ys
                                            else y : (insertPQL x ys)

    findMinPQ :: Ord a => PriorityQueue a -> a
    findMinPQ             (PriorityQueue xs) = head xs

    deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
    deleteMinPQ             (PriorityQueue xs) = (PriorityQueue (tail xs))