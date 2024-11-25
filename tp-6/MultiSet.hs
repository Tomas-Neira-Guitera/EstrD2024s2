module MultiSet


    (MultiSet, emptyMS, addMS, ocurrencesMS, multiSetToList)

    where 
    
    import Map
        
    data MultiSet a = MultiSet (Map a Int)

    emptyMS :: MultiSet a
    emptyMS = MultiSet emptyM                                                                                   -- O(1)

    addMS :: Ord a => a -> MultiSet a -> MultiSet a
    addMS             x    (MultiSet map) = if elem x (keys map)
                                                then assocM x (1 + (fromJust(lookupM x map))) (deleteM x map)
                                                else assocM x 1 map                                             -- O(n) donde n son la cantidad de elementos del map

    ocurrencesMS :: Ord a => a -> MultiSet a -> Int
    ocurrencesMS             x    (MultiSet map) = if elem (keys map)
                                                    then (fromJust(lookupM x map))
                                                    else 0                                                      -- O(n) donde n son la cantidad de elementos del map

    multiSetToList :: MultiSet a -> [(a, Int)]        
    multiSetToList    (MultiSet map) = mapToList map                                                            -- O(n) donde n son la cantidad de elementos del map

    mapToList :: Eq k => Map k v -> [(k, v)]
    mapToList            map     = juntarClavesYValores (keys map) (valuesM map)        -- La funcion valuesM me da los valores en el orden correcto ya que para implementarla utilizo "keys"

    -- Precondicon: las dos listas tiene la misma cantidad de elementos
    juntarClavesYValores :: [k] -> [Maybe v] -> [(k,v)]
    juntarClavesYValores    []     _      = []
    juntarClavesYValores    (k:ks) (v:vs) = (k,fromJust v) : (juntarClavesYValores ks vs)