data Color = Azul | Rojo
    deriving Show

data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas    c        CeldaVacia    = 0 
nroBolitas    c        (Bolita c2 ce) = if (esElMismoColor c c2)
                                            then 1 + (nroBolitas c ce)
                                            else nroBolitas c ce

esElMismoColor :: Color -> Color -> Bool
esElMismoColor    Azul     Azul  = True
esElMismoColor    Rojo     Rojo  = True
esElMismoColor    _        _     = False

poner :: Color -> Celda -> Celda 
poner    c        ce    = (Bolita c ce)

sacar :: Color -> Celda -> Celda 
sacar    _        CeldaVacia = CeldaVacia
sacar    c        (Bolita color celda) = if (esElMismoColor color c)
                                            then celda  
                                            else Bolita color (sacar c celda)

ponerN :: Int -> Color -> Celda -> Celda 
ponerN    0      _        celda = celda       
ponerN    n      c        celda = Bolita c (ponerN (n-1) c celda)
 
----------------------------------------------------------------------------

data Objeto = Cacharro | Tesoro
    deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

hayTesoro :: Camino -> Bool
hayTesoro    Fin            = False
hayTesoro    (Nada c)       = hayTesoro c
hayTesoro    (Cofre objs c) = (hayTesoroObjs objs) || hayTesoro c 

hayTesoroObjs :: [Objeto] -> Bool
hayTesoroObjs    []       = False
hayTesoroObjs    (o:objs) = case o of
                            Tesoro   -> True
                            Cacharro -> hayTesoroObjs objs

pasosHastaTesoro :: Camino -> Int
-- Precondicion: hay tesoro en el camino.
pasosHastaTesoro    Fin            = error "No cumple la precondicion"
pasosHastaTesoro    (Nada c)       = 1 + (pasosHastaTesoro c) 
pasosHastaTesoro    (Cofre objs c) = if hayTesoroObjs objs
                                        then 0
                                        else 1 + (pasosHastaTesoro c) 

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn    0      c           = hayTesoro c
hayTesoroEn    n      Fin         = False
hayTesoroEn    n      (Nada c)    = hayTesoroEn (n-1) c
hayTesoroEn    n      (Cofre _ c) = hayTesoroEn (n-1) c


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros    0      c              = True    
alMenosNTesoros    n      Fin            = False
alMenosNTesoros    n      (Nada c)       = alMenosNTesoros n c
alMenosNTesoros    n      (Cofre objs c) = if (hayTesoroObjs objs)
                                            then alMenosNTesoros (n-1) c 
                                            else alMenosNTesoros n c 

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- Precondicion el primer numero es menor al segundo.
cantTesorosEntre    0      0       _              = 0
cantTesorosEntre    0      n2      (Nada c)       = cantTesorosEntre 0 (n2-1) c 
cantTesorosEntre    0      n2      (Cofre objs c) = if (hayTesoroObjs objs)
                                                        then 1 + (cantTesorosEntre 0 (n2-1) c)
                                                        else cantTesorosEntre 0 (n2-1) c 
cantTesorosEntre    _      n2      Fin            = 0
cantTesorosEntre    n      n2      (Nada c)       = cantTesorosEntre (n-1) (n2-1) c
cantTesorosEntre    n      n2      (Cofre objs c) = cantTesorosEntre (n-1) (n2-1) c

----------------------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

arbol1  =  NodeT 1
        (NodeT 2
            (NodeT 4 EmptyT EmptyT)
            (NodeT 5 EmptyT EmptyT)
        )
        (NodeT 3
            (NodeT 6 (NodeT 10 EmptyT EmptyT) EmptyT)
            (NodeT 7 EmptyT EmptyT)
        )

sumarT :: Tree Int -> Int
sumarT    EmptyT          = 0
sumarT    (NodeT n ti td) = n + (sumarT ti) + (sumarT td)

sizeT :: Tree a -> Int
sizeT    EmptyT          = 0
sizeT    (NodeT n ti td) = 1 + (sizeT ti) + (sizeT td)

mapDobleT :: Tree Int -> Tree Int
mapDobleT    EmptyT          = EmptyT
mapDobleT    (NodeT n ti td) = NodeT (n * 2) (mapDobleT ti) (mapDobleT td) 

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT            x    EmptyT          = False
perteneceT            x    (NodeT y ti td) = if x == y
                                                then True
                                                else (perteneceT x ti) || (perteneceT x td)

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT            x    EmptyT          = 0
aparicionesT            x    (NodeT y ti td) = if x == y
                                                then 1 + (aparicionesT x ti) + (aparicionesT x td)
                                                else (aparicionesT x ti) + (aparicionesT x td)

leaves :: Tree a -> [a]
leaves    EmptyT                  = []
leaves    (NodeT x EmptyT EmptyT) = [x]             -- CREO QUE ES DOBLE PATERN MATCHING
leaves    (NodeT _ ti td)         = (leaves ti) ++ (leaves td)

heightT :: Tree a -> Int
heightT    EmptyT          = 0 
heightT    (NodeT y ti td) = 1 + max (heightT ti) (heightT td)

mirrorT :: Tree a -> Tree a
mirrorT    EmptyT          = EmptyT
mirrorT    (NodeT y ti td) = (NodeT y (mirrorT td) (mirrorT ti))

toList :: Tree a -> [a]
toList    EmptyT          = []
toList    (NodeT x ti td) = (toList ti) ++ [x] ++ (toList td)

levelN :: Int -> Tree a -> [a]
levelN    0      (NodeT x _ _)   = [x]         
levelN    _      EmptyT          = []
levelN    n      (NodeT _ ti td) = (levelN (n-1) ti) ++ (levelN (n-1) td)

listPerLevel :: Tree a -> [[a]]
listPerLevel    EmptyT           = []
listPerLevel    (NodeT x r2 r3) =  [x] : (mezclarListas (listPerLevel r2) (listPerLevel r3))

mezclarListas :: [[a]] -> [[a]] -> [[a]]
mezclarListas    []       y      = y
mezclarListas    x        []     = x
mezclarListas    (x:xs)   (y:ys) = (x ++ y) : mezclarListas xs ys

ramaMasLarga :: Tree a -> [a]
ramaMasLarga    EmptyT           = []
ramaMasLarga    (NodeT x r2 r3) = x : laMasLarga (ramaMasLarga r2) (ramaMasLarga r3)

laMasLarga :: [a] -> [a] -> [a]
laMasLarga    x      y   = if longitud x > longitud y
                            then x 
                            else y 

longitud :: [a] -> Int
longitud    []     = 0
longitud    (x:xs) = 1 + longitud xs

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos    EmptyT           = []
todosLosCaminos    (NodeT x r2 r3) = (agregar x (todosLosCaminos r2 ++ todosLosCaminos r3))

agregar :: a -> [[a]] -> [[a]]
agregar    x    []     = [[x]]
agregar    x    (y:ys) = (x:y) : agregar x ys

---------------------------------------------------------------------------------------------------------------------------

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

eval :: ExpA -> Int
eval (Valor x) = x
eval (Sum exp2 exp3) = (eval exp2)+(eval exp3)
eval (Prod exp2 exp3) = (eval exp2)*(eval exp3)
eval (Neg exp2) = -(eval exp2)

simplificar :: ExpA -> ExpA
simplificar (Valor x)     = Valor x
simplificar (Sum exp2 exp3)  = simplificacionSum  (simplificar exp3) (simplificar exp2)
simplificar (Prod exp2 exp3) = simplificacionProd (simplificar exp3) (simplificar exp2)
simplificar (Neg exp2)      = simplificacionNeg  (simplificar exp2)

simplificacionSum :: ExpA -> ExpA -> ExpA
simplificacionSum (Valor 0)  exp       = exp
simplificacionSum exp2       (Valor 0) = exp2
simplificacionSum exp2       exp3      = Sum exp2 exp3

simplificacionProd :: ExpA -> ExpA -> ExpA
simplificacionProd (Valor 0)  _         = Valor 0
simplificacionProd _          (Valor 0) = Valor 0
simplificacionProd (Valor 1)  exp       = exp
simplificacionProd exp2        (Valor 1) = exp2
simplificacionProd exp2        exp3       = Prod exp2 exp3

simplificacionNeg :: ExpA -> ExpA
simplificacionNeg (Neg exp) = exp 
simplificacionNeg exp2       = Neg exp2
