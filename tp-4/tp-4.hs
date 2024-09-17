data Pizza = Prepizza
           | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int
    deriving Show

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas    Prepizza = 0
cantidadDeCapas    (Capa _ p) = 1 + (cantidadDeCapas p) 

armarPizza :: [Ingrediente] -> Pizza
armarPizza    []            = Prepizza
armarPizza    (i:is)        = (Capa i (armarPizza is))

sacarJamon :: Pizza -> Pizza
sacarJamon    Prepizza   = Prepizza
sacarJamon    (Capa i p) = case i of
                            Jamon -> sacarJamon p
                            otherwise -> (Capa i (sacarJamon p))

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso    Prepizza   = True
tieneSoloSalsaYQueso    (Capa i p) = case i of
                            Salsa -> (tieneSoloSalsaYQueso p)
                            Queso -> (tieneSoloSalsaYQueso p)
                            otherwise -> False

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas    Prepizza   = Prepizza
duplicarAceitunas    (Capa i p) = case i of
                            Aceitunas n -> (Capa (Aceitunas (n*2)) (duplicarAceitunas p))
                            otherwise -> (Capa i (duplicarAceitunas p))

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza    []     = []
cantCapasPorPizza    (p:ps) = (cantidadDeCapas p, p) : (cantCapasPorPizza ps)

-----------------------------------------------------------------------------------------------------
data Dir = Izq | Der
    deriving Show

data Objeto = Tesoro | Chatarra
    deriving Show

data Cofre = Cofre [Objeto]
    deriving Show

data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
    deriving Show

hayTesoro :: Mapa -> Bool
hayTesoro    (Fin c)               = hayTesoroC c
hayTesoro    (Bifurcacion c m2 m3) = if hayTesoroC c
                                        then True
                                        else hayTesoro m2 || hayTesoro m3

hayTesoroC :: Cofre -> Bool
hayTesoroC    (Cofre obs) = hayTesoroL obs

hayTesoroL :: [Objeto] -> Bool
hayTesoroL    []      = False
hayTesoroL    (o:obs) = case o of
                        Tesoro -> True
                        otherwise -> hayTesoroL obs

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn    []       (Fin c)               = hayTesoroC c
hayTesoroEn    (d:ds)   (Fin c)               = False
hayTesoroEn    (d:ds)   (Bifurcacion c m2 m3) = case d of
                                                    Izq -> hayTesoroEn ds m2
                                                    Der -> hayTesoroEn ds m3

caminoAlTesoro :: Mapa -> Maybe [Dir]
-- Precondicion: existe un tesoro y es único.
caminoAlTesoro  (Fin c)               = Just []
caminoAlTesoro  (Bifurcacion c m2 m3) = if hayTesoroC c 
                                           then Just []
                                           else appendMaybe (consMaybe Izq (caminoAlTesoro m2)) (consMaybe Der (caminoAlTesoro m3))

consMaybe :: a -> Maybe [a] -> Maybe [a]
consMaybe    a    Nothing   = Nothing
consMaybe    a    (Just as) = Just (a:as)

appendMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
appendMaybe    Nothing      Nothing   = Nothing
appendMaybe    (Just xs)    Nothing   = Just xs 
appendMaybe    Nothing      (Just xs) = Just xs 
appendMaybe    (Just xs)    (Just ys) = Just (xs ++ ys) 

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga    (Fin _)               = []
caminoDeLaRamaMasLarga    (Bifurcacion _ m2 m3) = if (length (caminoDeLaRamaMasLarga m2)) > (length (caminoDeLaRamaMasLarga m3))
                                                        then Izq : (caminoDeLaRamaMasLarga m2)
                                                        else Der : (caminoDeLaRamaMasLarga m3)

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel    (Fin c)               = (tesorosDe c) : [] 
tesorosPorNivel    (Bifurcacion c mi md) = (tesorosDe c) : (mezclarListas (tesorosPorNivel mi) (tesorosPorNivel md))

tesorosDe :: Cofre -> [Objeto]
tesorosDe    (Cofre xs) = tesorosDeL xs

tesorosDeL :: [Objeto] -> [Objeto]
tesorosDeL    []      = []
tesorosDeL    (o:obs) = case o of
                        Tesoro    -> o : (tesorosDeL obs)
                        otherwise -> tesorosDeL obs

mezclarListas :: [[a]] -> [[a]] -> [[a]]
mezclarListas    []       y      = y
mezclarListas    x        []     = x
mezclarListas    (x:xs)   (y:ys) = (x ++ y) : mezclarListas xs ys

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos    (Fin c)               = []
todosLosCaminos    (Bifurcacion c mi md) = (consACada Izq (todosLosCaminos mi)) ++ (consACada Der (todosLosCaminos md))

consACada :: a -> [[a]] -> [[a]]
consACada    x    []     = []
consACada    x    (y:ys) = (x:y) : (consACada x ys)

-----------------------------------------------------------------------------------------------------
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show

data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
    -- Inv-rep: El campo SectorId no puede repetirse dentro de una misma nave.

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

data Nave = N (Tree Sector)
    deriving Show

sectores :: Nave -> [SectorId]
sectores    (N ts) = sectoresT ts

sectoresT :: Tree Sector -> [SectorId]
sectoresT    EmptyT          = []
sectoresT    (NodeT s ti td) = (sectorId s) : ((sectoresT ti) ++ (sectoresT td))

sectorId :: Sector -> SectorId
sectorId    (S id _ _) = id

poderDePropulsion :: Nave -> Int
poderDePropulsion    (N ts) = poderDePropulsionT ts

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT    EmptyT          = 0
poderDePropulsionT    (NodeT s ti td) = (poderDePropulsionS s) + (poderDePropulsionT ti) + (poderDePropulsionT td)

poderDePropulsionS :: Sector -> Int
poderDePropulsionS    (S _ cs _) = poderDePropulsionC cs

poderDePropulsionC :: [Componente] -> Int
poderDePropulsionC    []     = 0
poderDePropulsionC    (c:cs) = case c of
                                Motor p   -> p + (poderDePropulsionC cs)
                                otherwise -> (poderDePropulsionC cs)

barriles :: Nave -> [Barril]
barriles    (N ts) = barrilesDelLosSectores ts

barrilesDelLosSectores :: Tree Sector -> [Barril]
barrilesDelLosSectores    EmptyT                   = []
barrilesDelLosSectores    (NodeT unSector ts2 ts3) = barrilesDelSector unSector ++ barrilesDelLosSectores ts2 ++ barrilesDelLosSectores ts3

barrilesDelSector :: Sector -> [Barril]
barrilesDelSector    (S sectorId componentes tripulantes) = barrilesDeLosComponentes componentes

barrilesDeLosComponentes :: [Componente] -> [Barril]
barrilesDeLosComponentes    []      = []
barrilesDeLosComponentes    (c :cs) = case c of
                                Almacen b -> b ++ (barrilesDeLosComponentes cs)
                                otherwise -> barrilesDeLosComponentes cs

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector    cs              id          (N ts) = N (agregarASectorT cs id ts)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector 
agregarASectorT    _               _           EmptyT                   = EmptyT
agregarASectorT    cs              id          (NodeT unSector tsi tsd) = if id == (sectorId unSector)
                                                                            then (NodeT (agregarASectorS unSector cs) tsi tsd)
                                                                            else (NodeT unSector (agregarASectorT cs id tsi) (agregarASectorT cs id tsd))

agregarASectorS :: Sector -> [Componente] -> Sector
agregarASectorS    (S id cps ts) cs       = (S id (cps ++ cs) ts)

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tripulante sectores (N ts) = (N (asignarTripulanteAT tripulante sectores ts))

asignarTripulanteAT :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteAT    tripulante    []             ts                       = ts
asignarTripulanteAT    tripulante    _              EmptyT                   = EmptyT
asignarTripulanteAT    tripulante    (s:ss)         (NodeT unSector ts2 ts3) = if (s == sectorId unSector)
                                                                                                    then (NodeT (agregarAlTripulante tripulante unSector) (asignarTripulanteAT tripulante ss ts2) (asignarTripulanteAT tripulante ss ts3))
                                                                                                    else (NodeT unSector (asignarTripulanteAT tripulante ss ts2) (asignarTripulanteAT tripulante ss ts3))

agregarAlTripulante :: Tripulante -> Sector -> Sector
agregarAlTripulante tripulante (S id componentes tripulantes) = (S id componentes (tripulante : tripulantes))


sectoresAsignados ::  Tripulante -> Nave -> [SectorId]
sectoresAsignados     tripulante    (N ts) = sectoresQueContenganAlTripulante tripulante ts

sectoresQueContenganAlTripulante :: Tripulante -> Tree Sector -> [SectorId]
sectoresQueContenganAlTripulante    tripulante    EmptyT                   = []
sectoresQueContenganAlTripulante    tripulante    (NodeT unSector ts2 ts3) = if elSector_ContieneAlTripulante unSector tripulante
                                                                                then (sectorId unSector) : (sectoresQueContenganAlTripulante tripulante ts2) ++ (sectoresQueContenganAlTripulante tripulante ts3)
                                                                                else (sectoresQueContenganAlTripulante tripulante ts2) ++ (sectoresQueContenganAlTripulante tripulante ts3)

elSector_ContieneAlTripulante :: Sector -> Tripulante -> Bool
elSector_ContieneAlTripulante    (S id componentes tripulantes) tripulante = estaIncluidoEn tripulante tripulantes

estaIncluidoEn :: Tripulante -> [Tripulante] -> Bool
estaIncluidoEn    _             []           = False
estaIncluidoEn    tripulante    (t:ts)       = (tripulante == t) || (estaIncluidoEn tripulante ts)


tripulantes :: Nave -> [Tripulante]
tripulantes    (N ts) = tripulantesDe ts

tripulantesDe :: Tree Sector -> [Tripulante]
tripulantesDe    EmptyT                   = []
tripulantesDe    (NodeT unSector ts2 ts3) = agregarAlLosTripulantes_SinRepetirEn (tripulantesDelSector unSector) ((tripulantesDe ts2) ++ (tripulantesDe ts3))

agregarAlLosTripulantes_SinRepetirEn :: [Tripulante] -> [Tripulante] -> [Tripulante]
agregarAlLosTripulantes_SinRepetirEn    []              t2s          = t2s
agregarAlLosTripulantes_SinRepetirEn    ts              []           = ts
agregarAlLosTripulantes_SinRepetirEn    (t:ts)          t2s          =  if estaIncluidoEn t t2s
                                                                           then agregarAlLosTripulantes_SinRepetirEn ts t2s
                                                                           else t : agregarAlLosTripulantes_SinRepetirEn ts t2s

tripulantesDelSector :: Sector -> [Tripulante] 
tripulantesDelSector    (S id componentes tripulantes) = tripulantes

---------------------------------------------------------------------------------------------------------------------------
type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
    deriving Show

data Manada = M Lobo
    deriving Show

manada1 :: Manada 
manada1 = (M (Cazador "Cazador" ["presa"] 
                (Explorador "Explorador1" ["territorio1"] 
                    (Cria "cria2") (Cria "cria3") ) 
                (Explorador "Explorador2" ["territorio2"] 
                    (Cria "cria4") (Cria "cria5") )
                (Cria "cria1")))

buenaCaza :: Manada -> Bool
buenaCaza    (M l)  = (cantidadDeCrias l) < (cantidadDeAlimento l) 

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias    (Cazador _ _ l2 l3 l4) = (cantidadDeCrias l2) + (cantidadDeCrias l3) + (cantidadDeCrias l4)
cantidadDeCrias    (Explorador _ _ l2 l3) = (cantidadDeCrias l2) + (cantidadDeCrias l3)
cantidadDeCrias    (Cria _)               = 1

cantidadDeAlimento :: Lobo -> Int
cantidadDeAlimento    (Cazador _ ps l2 l3 l4) = length ps + (cantidadDeAlimento l2) + (cantidadDeAlimento l3) + (cantidadDeAlimento l4)
cantidadDeAlimento    (Explorador _ _ l2 l3)  = (cantidadDeAlimento l2) + (cantidadDeAlimento l3)
cantidadDeAlimento    (Cria _)                = 0


elAlfa :: Manada -> (Nombre, Int)
elAlfa    (M l)  = elAlfaL l 

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL    (Cria n)                = (n, 0)
elAlfaL    (Explorador n _ l2 l3)  = getAlfa (elAlfaL l2) (elAlfaL l3)
elAlfaL    (Cazador n ps l2 l3 l4) = getAlfa (n, length ps) (getAlfa (elAlfaL l2) (getAlfa (elAlfaL l3) (elAlfaL l4)))

getAlfa :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
getAlfa    (n, c)           (n2, c2)      = if c > c2
                                                then (n, c)
                                                else (n2, c2)    

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron    t             (M l)  = losQueExploraronL t l 

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL    _             (Cria _)                = [] 
losQueExploraronL    t             (Cazador _ _ l2 l3 l4)  = (losQueExploraronL t l2) ++ (losQueExploraronL t l3) ++ (losQueExploraronL t l4)
losQueExploraronL    t             (Explorador n ts l2 l3) = if elem t ts
                                                                then n : ((losQueExploraronL t l2) ++ (losQueExploraronL t l3)) 
                                                                else (losQueExploraronL t l2) ++ (losQueExploraronL t l3) 

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio    (M l)  = exploradoresPorTerritorioL l 

exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL    (Cria _)                = []
exploradoresPorTerritorioL    (Cazador _ _ l2 l3 l4)  = juntarNombresPorTerritorio (exploradoresPorTerritorioL l2) (juntarNombresPorTerritorio (exploradoresPorTerritorioL l3) (exploradoresPorTerritorioL l4))
exploradoresPorTerritorioL    (Explorador n ts l2 l3) =  agregarLoboCon n ts (juntarNombresPorTerritorio (exploradoresPorTerritorioL l2) (exploradoresPorTerritorioL l3))

juntarNombresPorTerritorio :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarNombresPorTerritorio    []                          tns2                     = tns2
juntarNombresPorTerritorio    tns1                         []                      = tns1
juntarNombresPorTerritorio    ((t,ns):ts)                 ((t2,ns2):ts2)           = if t == t2
                                                                                        then (t, ns ++ ns2) : (juntarNombresPorTerritorio ts ts2)
                                                                                        else  (t, ns) : (juntarNombresPorTerritorio ts ts2)

agregarLoboCon :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarLoboCon    _         []              tns                      = tns
agregarLoboCon    n         (t:ts)          []                       = (t, [n]) : (agregarLoboCon n ts [])
agregarLoboCon    n         (t:ts)          ((t2,ns):tns)            = if t == t2
                                                                            then (t, n:ns) : (agregarLoboCon n ts tns)
                                                                            else (agregarLoboCon n (t:ts) tns)

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador    n         (M lobo) = fromJust (superioresDelCazadorL n lobo)

superioresDelCazadorL :: Nombre -> Lobo -> Maybe [Nombre]
superioresDelCazadorL    _         (Cria _)               = Nothing
superioresDelCazadorL    n         (Explorador _ _ l2 l3) = juntarListas (superioresDelCazadorL n l2) (superioresDelCazadorL n l3)
superioresDelCazadorL    nombre    (Cazador n _ l2 l3 l4) = if nombre == n 
                                                                then (Just [])
                                                                else juntarListas (consMaybe n (superioresDelCazadorL nombre l2)) 
                                                                                  (juntarListas (consMaybe n (superioresDelCazadorL nombre l3)) (consMaybe n (superioresDelCazadorL nombre l4)))

juntarListas :: Maybe [Nombre] -> Maybe [Nombre] -> Maybe [Nombre]
juntarListas    Nothing           Nothing        = Nothing
juntarListas    (Just ns1)        Nothing        = (Just ns1)
juntarListas    Nothing           (Just ns2)     = (Just ns2)
juntarListas    (Just ns1)        (Just ns2)     = (Just (ns1 ++ ns2))

consMaybe :: Nombre -> Maybe [Nombre] -> Maybe [Nombre]
consMaybe    n         Nothing        = Nothing
consMaybe    n         (Just ns)      = (Just (n:ns))

fromJust :: Maybe a -> a 
fromJust    (Just a) = a