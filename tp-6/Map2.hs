module Map2

    (Map, emptyM, assocM, lookupM, deleteM, keys)

    where 
        data Map k v = Map [(k,v)]                  --- k se puede repetir.

    emptyM :: Map k v
    emptyM =  Map []                                                                 -- Costo O(1)

    assocM :: Eq k => k -> v -> Map k v -> Map k v
    assocM            k    v    (Map kvs) = Map ((k, v) : kvs)                       -- Costo O(1)


    lookupM :: Eq k => k -> Map k v -> Maybe v
    lookupM            k    (Map kvs) = encontrarEn k kvs                             -- Costo O(n) donde n en el peor caso es la longitud de kvs

    
    encontrarEn :: Eq k => k -> [(k, v)] -> Maybe v
    encontrarEn            k    []               = Nothing
    encontrarEn            k    ((k2, v2) : kvs) = if k == k2
                                                    then Just v2
                                                    else (encontrarEn k kvs)            -- Costo O(n) donde n en el peor caso es la longitud de la lista.


    deleteM :: Eq k => k -> Map k v -> Map k v
    deleteM            k    (Map kvs) = eliminar k kvs                                  -- Costo O(n) donde n en el peor caso es la longitud de kvs

    eliminar :: Eq k => k -> [(k, v)] -> [(k, v)]
    eliminar            k    []               = []
    eliminar            k    ((k2, v2) : kvs) = if k == k2
                                                    then eliminar kvs
                                                    else (k2, v2) : (eliminar k kvs)    -- Costo O(n) donde n en el peor caso es la longitud de la lista.

    keys :: Map k v -> [k]
    keys    (Map kvs) = clavesSinRepedtidos kvs                                         -- Costo O(n) donde n es la longitud de kvs.

    clavesSinRepedtidos :: [(k, v)] -> [k] 
    clavesSinRepedtidos    []            = []
    clavesSinRepedtidos    ((k,v) : kvs) = if elem k (clavesSinRepedtidos kvs)
                                            then clavesSinRepedtidos kvs
                                            else k : clavesSinRepedtidos kvs            -- Costo O(n) donde n es la longitud de la lista.