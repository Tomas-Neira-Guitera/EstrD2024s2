sumatoria :: [Int] -> Int
sumatoria    []     = 0   
sumatoria   (n:ns)  = n + sumatoria ns

longitud :: [a] -> Int
longitud    []     = 0
longitud    (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores    []     = error "el vacio no tiene sucesor"
sucesores    (n:ns) = (n + 1) : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion      []   = True
conjuncion    (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
disyuncion      []   =  False
disyuncion    (b:bs) = b || disyuncion bs  

aplanar :: [[a]] -> [a]
aplanar     []    = []
aplanar    (x:xs) = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece            _   []   = False
pertenece            e (x:xs) = (e == x)  || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones            a    []     = 0
apariciones            a    (x:xs) = if a == x
                                        then 1 + (apariciones a xs)
                                        else apariciones a xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA     _      []    = []
losMenoresA     n     (x:xs) = if x<n 
                                then  x : losMenoresA n xs
                                else    losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA    n      []       = []
lasDeLongitudMayorA    n      (xs:xss) = if length xs > n 
                                            then xs : (lasDeLongitudMayorA n xss)
                                            else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal    []     y = y : []
agregarAlFinal    (x:xs) y = x : (agregarAlFinal xs y)

agregar :: [a] -> [a] -> [a]
agregar    []     ys  = ys
agregar    (x:xs) ys  = x : (agregar xs ys)

reversa :: [a] -> [a] 
reversa    []     = []
reversa    (x:xs) = (reversa xs) ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos    []       ns2      = ns2
zipMaximos    ns1      []       = ns1
zipMaximos    (n:ns1)  (n2:ns2) = if n > n2
                                    then n : (zipMaximos ns1 ns2)
                                    else n2 : (zipMaximos ns1 ns2)

elMinimo :: Ord a => [a] -> a 
-- Precondicion: La lista no puede ser vacia.
elMinimo             (x:[]) = x
elMinimo             (x:xs) = let minimo = (elMinimo xs) in 
                                if minimo < x 
                                 then minimo
                                 else x 
------------------------------------------------------------------------------
{- EJERCIO 2 Recursión sobre numeros -}

factorial :: Int -> Int
factorial    0 = 1
factorial    n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva    0   = []
cuentaRegresiva    n   = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir    0      _ = []
repetir    n      e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros    _      []     = []
losPrimeros    n      (x:xs) = if n > 0
                                then x : losPrimeros (n-1) xs
                                else  losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros    0      y      = y
sinLosPrimeros    _      []     = []   
sinLosPrimeros    n      (x:xs) = sinLosPrimeros (n-1) xs

------------------------------------------------------------------------------
data Persona = P String Int 
    deriving Show

nahue   = (P "Nahuel"  21) 
joaco   = (P "Joaquin" 23)
augusto = (P "Augusto" 20)

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA    n      []     = []
mayoresA    n      (p:ps) = if edad p > n
                            then p : mayoresA n ps
                            else mayoresA n ps

edad :: Persona -> Int
edad    (P n e) = e

promedioEdad :: [Persona] -> Int
promedioEdad    []        = 0
promedioEdad    ps        = div (sumatoriaDeEdades ps) (longitud ps)

sumatoriaDeEdades :: [Persona] -> Int
sumatoriaDeEdades    []        = 0
sumatoriaDeEdades    (p:ps)    = edad p + sumatoriaDeEdades ps

elMasViejo :: [Persona] -> Persona
elMasViejo    [p]       = p 
elMasViejo    (p:ps)    = laQueEsMayor p (elMasViejo ps)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor    p           p2      = if(edad p > edad p2)
                                        then p
                                      else p2

{- EJERCIO 3-2 Registros -}

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

pokemon1    = (ConsPokemon Agua 30)
pokemon2    = (ConsPokemon Fuego 50)
pokemon3    = (ConsPokemon Planta 60)

entrenador1  = (ConsEntrenador "nacho" [pokemon1, pokemon2, pokemon3])
entrenador2 = (ConsEntrenador "tomi" [pokemon3])

cantPokemon :: Entrenador -> Int
cantPokemon    (ConsEntrenador nombre pokemones) = longitud pokemones

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe      tipo             (ConsEntrenador nombre pokemones) = cantindadDePokemonesDe tipo pokemones

cantindadDePokemonesDe :: TipoDePokemon -> [Pokemon] -> Int
cantindadDePokemonesDe    tipo             []        = 0
cantindadDePokemonesDe    tipo             (p:ps)    = if sonDelMismoTipo (tipoDe p) tipo
                                                            then 1 + cantindadDePokemonesDe tipo ps
                                                            else cantindadDePokemonesDe tipo ps

tipoDe :: Pokemon -> TipoDePokemon
tipoDe    (ConsPokemon tipo energia) = tipo

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo    Agua             Agua          = True
sonDelMismoTipo    Fuego            Fuego         = True
sonDelMismoTipo    Planta           Planta        = True
sonDelMismoTipo    _                _             = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_    tipo             e1            e2         =  cantPokemonDe tipo e1 * cantPokemonDe (tipoQuePierdeContra tipo) e2

tipoQuePierdeContra :: TipoDePokemon -> TipoDePokemon
tipoQuePierdeContra    Agua          = Fuego
tipoQuePierdeContra    Fuego         = Planta
tipoQuePierdeContra    Planta        = Agua

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon    (ConsEntrenador nombre pokemones) = hayAlgunPokemonDeTipo_En Agua pokemones && hayAlgunPokemonDeTipo_En Fuego pokemones && hayAlgunPokemonDeTipo_En Planta pokemones

hayAlgunPokemonDeTipo_En :: TipoDePokemon -> [Pokemon] -> Bool
hayAlgunPokemonDeTipo_En    tipo             []        =  False
hayAlgunPokemonDeTipo_En    tipo             (p:ps)    =  (sonDelMismoTipo (tipoDe p) tipo) || hayAlgunPokemonDeTipo_En tipo ps 

{- EJERCIO 3-1 Registros -}

data Seniority = Junior | SemiSenior | Senior
  deriving Show

data Proyecto = ConsProyecto String
  deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
  deriving Show

data Empresa = ConsEmpresa [Rol]
  deriving Show

proyecto1 = (ConsProyecto "proyecto 1")
proyecto2 = (ConsProyecto "proyecto 2")

rol1 = (Developer SemiSenior proyecto1)
rol2 = (Management Junior proyecto2)
rol3 = (Developer SemiSenior proyecto1)
rol4 = (Management Junior proyecto2)

empresa1 = (ConsEmpresa [])
empresa2 = (ConsEmpresa [rol1, rol2, rol3, rol4])

----------------------------------------------------------------------------------------------------------

proyectos :: Empresa -> [Proyecto]
proyectos    (ConsEmpresa roles) = proyectosDe roles

proyectosDe :: [Rol] -> [Proyecto]
proyectosDe    []     = []
proyectosDe    (r:rs) = if elProyectoSeEncuentraEn (proyecto r) rs
                          then proyectosDe rs
                          else proyecto r : proyectosDe rs

elProyectoSeEncuentraEn :: Proyecto -> [Rol] -> Bool
elProyectoSeEncuentraEn    _           []         = False
elProyectoSeEncuentraEn    p1          (r:rs)     = (nombre p1) == (nombre (proyecto r)) || elProyectoSeEncuentraEn p1 rs

proyecto :: Rol -> Proyecto
proyecto    (Developer s p) = p
proyecto    (Management s p) = p 

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior    (ConsEmpresa roles) ps = cantDeDeveloperSeniorDe_QueTrabajanEn_ roles ps

cantDeDeveloperSeniorDe_QueTrabajanEn_ :: [Rol] -> [Proyecto] -> Int
cantDeDeveloperSeniorDe_QueTrabajanEn_    []        ps        = 0
cantDeDeveloperSeniorDe_QueTrabajanEn_    (r:rs)    ps        = if elRolEsDeveloperSenior r && perteneceALosProyectosDe r ps
                                                                  then 1 + cantDeDeveloperSeniorDe_QueTrabajanEn_ rs ps
                                                                  else cantDeDeveloperSeniorDe_QueTrabajanEn_ rs ps

elRolEsDeveloperSenior :: Rol -> Bool
elRolEsDeveloperSenior    (Developer Senior _)  = True
elRolEsDeveloperSenior    _                     = False

perteneceALosProyectosDe :: Rol -> [Proyecto] -> Bool
perteneceALosProyectosDe    r      []         = False
perteneceALosProyectosDe    r      (p:ps)     = nombre p == nombre (proyecto r) || perteneceALosProyectosDe r ps 

nombre :: Proyecto -> String
nombre    (ConsProyecto n)  = n 

----------------------------------------------------------------------------------------------------------

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps e = cantidadDeEmpleadosDe_QueTrabajanEnAlgunProyectoDe_ (empleados e) ps

empleados :: Empresa -> [Rol]
empleados (ConsEmpresa rs) = rs

cantidadDeEmpleadosDe_QueTrabajanEnAlgunProyectoDe_ :: [Rol] -> [Proyecto] -> Int
cantidadDeEmpleadosDe_QueTrabajanEnAlgunProyectoDe_ [] ps = 0
cantidadDeEmpleadosDe_QueTrabajanEnAlgunProyectoDe_ (r:rs) ps = if perteneceALosProyectosDe r ps
                                                                  then 1 + cantidadDeEmpleadosDe_QueTrabajanEnAlgunProyectoDe_ rs ps
                                                                  else cantidadDeEmpleadosDe_QueTrabajanEnAlgunProyectoDe_ rs ps

----------------------------------------------------------------------------------------------------------

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = tuplasDeProyectosConEmpleadosDe rs

tuplasDeProyectosConEmpleadosDe :: [Rol] -> [(Proyecto, Int)]
tuplasDeProyectosConEmpleadosDe    []     = []
tuplasDeProyectosConEmpleadosDe    (r:rs) = incluirAlEmpleado_En r (tuplasDeProyectosConEmpleadosDe rs)

incluirAlEmpleado_En :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
incluirAlEmpleado_En    r      []                = [(proyecto r, 1)]
incluirAlEmpleado_En    r      (pc:pcs)          = if nombre (proyecto r) == (nombre (fst pc))
                                                    then ((fst pc), (snd pc) + 1) : pcs
                                                    else pc : incluirAlEmpleado_En r pcs


