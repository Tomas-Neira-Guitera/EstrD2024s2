module Set 

    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
    
    where

    data Set a = Set [a] Int
    -- Inv-rep: * Si la lista del set se encuentra vacia entonces el int es 0.
    --          * El Int representa la cantidad de elementos de la lista.
    --          * El Int no puede ser < 0.


    emptyS :: Set a 
    emptyS = (Set [] 0)                                                         -- O(1)

    addS :: Eq a => a -> Set a -> Set a
    addS            a    (Set xs int) = if elem a xs
                                            then (Set xs int)
                                            else (Set (a : xs) (int + 1))       -- O(n) donde n son la cantidad de elementos del set.

    belongs :: Eq a => a -> Set a -> Bool   
    belongs            a    (Set xs int) = elem a xs                            -- O(n) donde n son la cantidad de elementos del set.

    sizeS :: Eq a => Set a -> Int
    sizeS            (Set xs int) = int                                         -- O(1)

    removeS :: Eq a => a -> Set a -> Set a
    removeS            a    (Set xs int) = if elem a xs
                                            then (Set (removeL a xs) (int - 1))
                                            else (Set xs int)                   -- O(n) donde n son la cantidad de elementos del set.

    removeL :: Eq a => a -> [a] -> [a]
    removeL            _    []  = []
    removeL            x    (y:ys) = if x == y 
                                        then ys
                                        else x : (removeL x ys)                 -- O(n) donde n son la cantidad de elementos de la lista.

    unionS :: Eq a => Set a -> Set a -> Set a
    unionS            (Set xs int) set = unionSL xs set                         -- O(n) por el llamado a unionSL de costo n.

    unionSL :: Eq a => [a] -> Set a -> Set a 
    unionSL    []     set   = set
    unionSL    (x:xs) set   = addS x (unionSL xs set)                           -- O(n) donde n son la cantidad de elementos de la lista.

    setToList :: Eq a => Set a -> [a]
    setToList            (Set xs int) = xs                                      -- O(1) 

    
