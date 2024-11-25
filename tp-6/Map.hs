module Map

    (Map, emptyM, assocM, lookupM, deleteM, keys)

    where

    data Map k v = Map [(k,v)]
    -- Inv-Rep: cada valor de k es unico dentro de la lista de pares (k,v)


    emptyM :: Map k v
    emptyM =  (Map [])

    assocM :: Eq k => k -> v -> Map k v -> Map k v
    assocM            k    v    (Map kvs) = (Map (assocML k v kvs))

    assocML :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
    assocML            k    v    []              = [(k,v)]
    assocML            k    v    ((k2, v2): kvs) = if k == k2
                                                    then (k, v) : kvs
                                                    else (k2, v2) : (assocML k v kvs)

    lookupM :: Eq k => k -> Map k v -> Maybe v
    lookupM            k    (Map kvs) = lookupMl k kvs

    lookupMl :: Eq k => k -> [(k,v)] -> Maybe v 
    lookupMl            k    []               = Nothing
    lookupMl            k    ((k2, v2) : kvs) = if k == k2
                                                    then Just v2
                                                    else lookupMl k kvs

    deleteM :: Eq k => k -> Map k v -> Map k v
    deleteM            k    (Map kvs) = (Map (deleteMl k kvs))

    deleteMl :: Eq k => k -> [(k,v)] -> [(k,v)]
    deleteMl            k    []               = []
    deleteMl            k    ((k2, v2) : kvs) = if k == k2
                                                    then kvs
                                                    else (k2, v2) : (deleteMl k kvs)
    keys :: Map k v -> [k]
    keys    (Map kvs) = keysL kvs

    keysL :: [(k,v)] -> [k]
    keysL     []            = []
    keysL     ((k,v) : kvs) = k : (keysL kvs)
