
{-
-- Ejercicio 1
{-
Costos de las funciones de la interfaz:
insertPQ O(log n)
deleteMinPQ O(log n)
findMinPQ O(1)
isEmptyPQ O(1)
-}

heapSort :: Ord a => [a] -> [a]
heapSort             xs  = transformarAUnaLista (agregar xs emptyPQ)
{-
Costo: O((n * log m) + (n * log n))
Justificacion:
el costo de esta funcion es por el uso de las funciones 
trasformarAUnaLista O(n * log n)
agregar O(n * log m)
-}


agregar :: Ord a => [a] -> PriorityQueue a -> PriorityQueue a 
agregar    []     pq              = pq
agregar    (x:xs) pq              = insertPQ x (agregar xs pq)
{-
Costo: O(n * log m)
Donde n son la cantidad de elementos de la lista.
Donde m son la cantidad de elementos de la PQ.
Justificacion: por cada elemento de la lista se realizan las siguites operaciones:
insertPQ O(log n)
-}



transformarAUnaLista :: Ord a => PriorityQueue a -> [a]
transformarAUnaLista    pq              = if isEmptyPQ pq
                                            then []
                                            else findMinPQ pq : transformarAUnaLista (deleteMinPQ pq)
{-
Costo: O(n * log n)
Donde n son la cantidad de elementos de la PQ.
Justificacion del costo: ya que se realiza una recursion, por cada elemento de la PQ se realizan las siguientes funciones:
deleteMinPQ O(log n)
findMinPQ O(1)
isEmptyPQ O(1)
-}
-}

-------------------------------------------------------------------------------------------------------------------------------------------
{-
-- Ejercio 2 -- Todos los Tree a cumplen el INV-REP: BST
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 
    deriving Show

-- (insertBST 8 (insertBST 3 (insertBST 20 (insertBST 15 (insertBST 5 (insertBST 10 EmptyT))))))


belongsBST :: Ord a => a -> Tree a -> Bool      -- Costo: O(log N)
belongsBST             _    EmptyT          = False
belongsBST             x    (NodeT y t2 t3) = if x == y
                                                then True   
                                                    else if x < y  
                                                        then belongsBST x t2
                                                        else belongsBST x t3

insertBST :: Ord a => a -> Tree a -> Tree a     -- Costo: O(log N)
insertBST             x    EmptyT          = NodeT x EmptyT EmptyT
insertBST             x    (NodeT y t2 t3) = if x == y
                                                then (NodeT x t2 t3)   
                                                    else if x < y  
                                                        then NodeT y (insertBST x t2) t3
                                                        else NodeT y t2 (insertBST x t3)


deleteBST :: Ord a => a -> Tree a -> Tree a    -- Costo: O(log N)
deleteBST             x    EmptyT          = EmptyT
deleteBST             x    (NodeT y t2 t3) = if x == y
                                                then rearmarBST t2 t3   
                                                    else if x < y  
                                                        then NodeT y (deleteBST x t2) t3
                                                        else NodeT y t2 (deleteBST x t3)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a  
rearmarBST             ti        td     = case ti of 
                                            EmptyT    -> td
                                            otherwise -> let (m, ti') = splitMaxBST ti 
                                                            in (NodeT m ti' td)


root :: Tree a -> a 
-- Precondicion: El arbol que recibe debe ser un NodeT
root    (NodeT x t2 t3) = x 


splitMinBST :: Ord a => Tree a -> (a, Tree a)   -- Costo: O(log N) -- Proposito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- precondicion: el arbol no esta vacio.
splitMinBST             (NodeT x t2 t3)     = case t2 of
                                                EmptyT    -> (x, t3)
                                                otherwise -> let (m, t2') = (splitMinBST t2)    
                                                                in (m, NodeT x t2' t3)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)   -- Costo: O(log N)
-- precondicion: el arbol no esta vacio.
splitMaxBST             (NodeT x t2 t3) = case t3 of 
                                            EmptyT    -> (x, t2)
                                            otherwise -> let (m, t3') = (splitMaxBST t3)
                                                            in (m, NodeT x t2 t3')

esBST :: Ord a => Tree a -> Bool     -- Costo: O(N)                             -- PREGUNTAR SI FUNCIONA EL CIRCUITO CORTO Y SI EL COSTO DEBE SER N*N
esBST    EmptyT          = True
esBST    (NodeT x t2 t3) = esBST t2 && esBST t3 && ((esMasGrande x t2) && (esMasChico x t3))

esMasGrande :: Ord a => a -> Tree a -> Bool
esMasGrande             x    EmptyT          = True
esMasGrande             x    (NodeT y t2 t3) = x > y

esMasChico :: Ord a => a -> Tree a -> Bool
esMasChico             x    EmptyT          = True
esMasChico             x    (NodeT y t2 t3) = x < y 


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a   -- Costo: O(log N)
elMaximoMenorA             x    EmptyT          = Nothing
elMaximoMenorA             x    (NodeT y ti td) = if x <= y 
                                                    then elMaximoMenorA x ti
                                                    else Just (elMasGrandeEntre y (elMaximoMenorA x td))
-- La funcion debe retornar un elemento que sea menor a x pero el mas grande.

elMasGrandeEntre :: Ord a => a -> Maybe a -> a 
elMasGrandeEntre             x    Nothing  = x 
elMasGrandeEntre             _    (Just y) = y

elementoDe :: Tree a -> Maybe a 
elementoDe    EmptyT          = Nothing
elementoDe    (NodeT x t2 t3) = Just x

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a   -- Costo: O(log N)
elMinimoMayorA             x    EmptyT          = Nothing
elMinimoMayorA             x    (NodeT y ti td) = if x >= y 
                                                    then elMinimoMayorA x td
                                                    else Just (elMasChicoEntre y (elMinimoMayorA x ti))        -- x < y Puede ser que en y haya un elemento mas chico que y pero mas grande que x
-- La funcion debe retornar un elemento que sea mayor a x pero el mas chico.

elMasChicoEntre :: Ord a => a -> Maybe a -> a
elMasChicoEntre             x    Nothing  = x
elMasChicoEntre             x    (Just y) = min x y 

balanceado :: Tree a -> Bool
balanceado    EmptyT          = True
balanceado    (NodeT x ti td) = (abs ((heightT ti) - (heightT td) )) <= 1 && (balanceado ti) && (balanceado td)

heightT :: Tree a -> Int
heightT    Empty           = 0
heightT    (NodeT _ r2 r3) = 1 + max (heightT r2) (heightT r3)

-------------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 3

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM            map     = valoresDeConClave map (keys map)

-- Costo: O(n * log n + n)

valoresDeConClave :: Eq k => Map k v -> [k] -> [Maybe v]
valoresDeConClave            map        []     = []
valoresDeConClave            map        (k:ks) = lookupM k map : valoresDeConClave map ks

-- Costo: O(n * log m)
-- Justificacion: n son la cantidad de claves de la lista de claves, m son la cantidad de claves del map
-- Por cada clave de la lista de claves se realiza las siguientes funciones:
-- lookupM -> O(log m)


todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas            []     map     = True
todasAsociadas            (k:ks) map     = seEncuentraEn k (keys map) && (todasAsociadas ks map)

-- Costo O(n * m)
-- Justificacion: n son la cantidad de elementos de la lista de claves, m son la cantidad de claves del map.
-- por cada elemento de la lista de claves se realizan las siguientes funciones:
-- seEncuentraEn -> O(m)

seEncuentraEn :: Eq k => k -> [k] -> Bool
seEncuentraEn            x    []     = False
seEncuentraEn            x    (k:ks) = x == k  || (seEncuentraEn k ks)

-- Costo O(n)
-- Justificacion: n son la cantidad de claves de la lista de claves.
-- Por cada elemento se realiza la funcion:
-- == -> O(1)

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap            []           = emptyM
listToMap            ((k, v): ks) = assocM k v (listToMap ks)

-- Costo: O(n * (log n))
-- Justificacion: n son la cantidad de elementos de la lista de pares.
-- por cada elemento de la lista de pares se realizan las siguientes funciones:
-- assocM -> O(log n)

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

-------------------------------------------------------------------------------------------------------------------------------------------
-}
-- Ejercicio 5: Usuario de empresa.
import Empresa -- (ConsE, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)

comenzarCon :: [SectorId] -> [CUIL] -> Empresa -- Proposito: construye una empresa con la información de empleados dada.
comenzarCon    sectores      empleados = agregarEmpleados empleados sectores (agregarSectores sectores consEmpresa)

agregarASectores :: [SectorId] -> Empresa -> Empresa
agregarASectores    []            empresa = empresa
agregarASectores    (s:ss)        empresa = agregarSector s (agregarASectores ss empresa)

agregarEmpleados :: [CUIL] -> [SectorId] -> Empresa -> Empresa
agregarEmpleados    []        ss            empresa = empresa
agregarEmpleados    (c:cs)    ss            empresa = agregarEmpleado ss c (agregarEmpleados cs ss) 

recorteDePersonal :: Empresa -> Empresa  -- Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
recorteDePersonal    empresa = let empleados = todosLosCUIL empresa 
                                    cantidadDeEmpleados = length empleados in 
                                    recortarAEstosEmpleados (take (cantidadDeEmpleados/2) empleados) empresa

recortarAEstosEmpleados :: [CUIL] -> Empresa -> Empresa
recortarAEstosEmpleados    []        empresa = empresa
recortarAEstosEmpleados    (c:cs)    empresa = borrarEmpleado c (recortarAEstosEmpleados cs empresa)

convertirEnComodin :: CUIL -> Empresa -> Empresa -- Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
convertirEnComodin    cuil    empresa = agregarASectores cuil (todosLosSectores empresa) empresa

agregarASectores :: CUIL -> [SectorId] -> Empresa -> Empresa
agregarASectores    cuil    []            empresa = empresa          
agregarASectores    cuil    (s:ss)        empresa = agregarASector cuil s (agregarASectores cuil ss empresa) 

esComodin :: CUIL -> Empresa -> Bool  -- Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
esComodin    cuil    empresa = seEncuentraEnLosSectores (buscarPorCUIL cuil empresa) (todosLosSectores empresa)

seEncuentraEnLosSectores :: Empleado -> [SectorId] -> Bool
seEncuentraEnLosSectores    empleado    []         = True
seEncuentraEnLosSectores    empleado    (s:ss)     = elem empleado (empleadosDelSector s) && (seEncuentraEnLosSectores cuil ss)
