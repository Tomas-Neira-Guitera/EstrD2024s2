module Map3

    (Map, emptyM, assocM, lookupM, deleteM, keys)

    where 
        
    data Map k v = Map [k] [v]
        -- INV-REP: Las listas [k] [v] la clave con indice i en [k] esta relacionada al valor con indice i en [v].

    emptyM :: Map k v
    emptyM =  Map [] []                                                                  -- Costo O(1)

    assocM :: Eq k => k -> v -> Map k v -> Map k v
    assocM            k    v    (Map ks vs) = Map (k:ks) (v:vs)                         -- Costo O(1)

    lookupM :: Eq k => k -> Map k v -> Maybe v
    lookupM            k    (Map ks vs) = encontrar (indiceSiExisteDe k ks) vs              -- Costo O(n+n)

    indiceDeSiExisteEn :: Eq k => k -> ks -> Maybe Int 
    indiceDeSiExisteEn            x    []     = Nothing
    indiceDeSiExisteEn            x    (k:ks) = if x == k 
                                                 then Just 0
                                                 else sumarSi 1 (indiceDeSiExisteEn x ks)       -- Costo O(n * (1+1)) = O(n)

    sumarSi :: Int -> Maybe Int -> Maybe Int
    sumarSi    x      Nothing   = Nothing
    sumarSi    x      Just v    = Just (x + v)                          -- Costo O(1)
    
    encontrar :: Eq k => Maybe Int -> [v] -> Maybe v 
    encontrar            Nothing       _      = Nothing
    encontrar            Just 0        (v:vs) = Just v
    encontrar            Just n        (v:vs) = encontrar (Just (x-1)) vs           -- Costo O(n)

    deleteM :: Eq k => k -> Map k v -> Map k v
    deleteM            k    (Map ks vs) = Map if elem k ks
                                            then Map (eliminar k ks) (eliminarElemConIndice (indiceDeSiExisteEn k ks) vs)   -- Costo O(n + n + n) = O(3n) = O(n)
                                            else Map ks vs

    eliminar :: Eq k => k -> [k] -> [k]
    eliminar            x    []     = []
    eliminar            x    (k:ks) = if x == k
                                        then ks
                                        else k : (eliminar x ks)            -- Costo O(n)

    eliminarElemConIndice :: Maybe Int -> [v] -> [v]
    eliminarElemConIndice    Nothing       _     = []
    eliminarElemConIndice    Just 0       (v:vs) = vs
    eliminarElemConIndice    Just n       (v:vs) = v : (eliminarElemConIndice (Just (n-1)) vs)  -- Costo O(n)

    keys :: Map k v -> [k]
    keys    (Map ks vs) = ks                                                    -- Costo O(1)