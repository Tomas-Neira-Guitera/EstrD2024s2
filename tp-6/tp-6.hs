{-
import PriorityQueue        -- (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

-- Proposito: ordena la lista de menor a mayor utilizando una Priority Queue como estructura auxiliar.
heapSort :: Ord a => [a] -> [a]
heapSort             xs  = transformarAUnaLista (agregar xs emptyPQ)

agregar :: Ord a => [a] -> PriorityQueue a -> PriorityQueue a 
agregar    []     pq              = pq
agregar    (x:xs) pq              = insertPQ x (agregar xs pq)

transformarAUnaLista :: Ord a => PriorityQueue a -> [a]
transformarAUnaLista    pq              = if isEmptyPQ pq
                                            then []
                                            else findMinPQ pq : transformarAUnaLista (deleteMinPQ pq)
-}
-----------------------------------------------------------------------------------------------------------------
{-
import Map                 -- (Map, emptyM, assocM, lookupM, deleteM, keys)

unMap = assocM 7 "7" (assocM 6 "6" (assocM 5 "5" emptyM))


valuesM :: Eq k => Map k v -> [Maybe v]
valuesM            map     = valoresDeConClave map (keys map)

valoresDeConClave :: Eq k => Map k v -> [k] -> [Maybe v]
valoresDeConClave            map        []     = []
valoresDeConClave            map        (k:ks) = lookupM k map : valoresDeConClave map ks


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas            []     map     = True
todasAsociadas            (k:ks) map     = seEncuentraEn k (keys map) && (todasAsociadas ks map)

seEncuentraEn :: Eq k => k -> [k] -> Bool
seEncuentraEn            x    []     = False
seEncuentraEn            x    (k:ks) = x == k  || (seEncuentraEn k ks)

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap            []           = emptyM
listToMap            ((k, v): ks) = assocM k v (listToMap ks)

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList            map     = juntarClavesYValores (keys map) (valuesM map)        -- La funcion valuesM me da los valores en el orden correcto ya que para implementarla utilizo "keys"

-- Precondicon: las dos listas tiene la misma cantidad de elementos
juntarClavesYValores :: [k] -> [Maybe v] -> [(k,v)]
juntarClavesYValores    []     _      = []
juntarClavesYValores    (k:ks) (v:vs) = (k,fromJust v) : (juntarClavesYValores ks vs)

{-
-- Proposito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v] 
agruparEq            []            = emptyM
agruparEq            ((k,v) : kvs) = consolidar k v (agruparEq kvs)

consolidar :: Eq k => k -> v -> Map k [v] -> Map k [v]
consolidar            k    v    map = case (lookupM k map) of
                                        Just v2 -> assocM k (v:v2) map
                                        Nothing -> assocM k (v:[]) map
-}

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar            []     map       =  map
incrementar            (k:ks) map       =  if elem k (keys map)
                                            then assocM k ((fromJust (lookupM k map)) + 1) (incrementar ks map) -- con el elem k (keys map) me aseguro la utilizacion del fromJust.
                                            else incrementar ks map

fromJust :: Maybe v -> v 
fromJust    (Just v) = v


mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps           map1       map2     = agregarM (keys map1) map1 map2

agregarM :: Eq k => [k] -> Map k v -> Map k v -> Map k v
agregarM            []     map1       map2    = map2
agregarM            (k:ks) map1       map2    = assocM k (fromJust(lookupM k map1)) (agregarM ks map1 map2)


-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
indexar :: [a] -> Map Int a
indexar    xs  = agregarIndexandoAPartirDe xs 1 emptyM

agregarIndexandoAPartirDe :: [a] -> Int -> Map Int a -> Map Int a 
agregarIndexandoAPartirDe    []     n      map = map
agregarIndexandoAPartirDe    (x:xs) n      map = assocM n x (agregarIndexandoAPartirDe xs (n+1) map)

-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias    []     = emptyM
ocurrencias    (c:cs) = if elem c (keys(ocurrencias cs))
                            then assocM c (1 + (fromJust (lookupM c (ocurrencias cs)))) (ocurrencias cs)
                            else assocM c 1 (ocurrencias cs)
-}
-----------------------------------------------------------------------------------------------------------------

-- Propósito: dado un string, devuelve un MultiSet donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> MultiSet a
ocurrencias    []     = emptyMS
ocurrencias    (c:cs) = addMS c (ocurrencias cs)
