{- EJERCIO 2 Números enteros -}

-- 1)

sucesor :: Int -> Int
sucesor     x  = x + 1

sumar :: Int -> Int -> Int
sumar    n      m   = n + m 

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto    n      m   = (div n m, mod n m )

maxDelPar :: (Int, Int) -> Int
maxDelPar    (n, m)     = if(n > m)
                           then n 
                          else m 

-- 2)

-- sumar maxDelPar(8, 2) (maxDelPar (divisionYResto 2 1))

{- EJERCIO 3 Tipos enumerativos -}

-- 1)

data Direccion = Norte | Este | Sur | Oeste

    deriving Show 

opuesto :: Direccion -> Direccion
opuesto    Norte     =  Sur
opuesto    Este      =  Oeste
opuesto    Sur       =  Norte
opuesto    Oeste     =  Este

iguales :: Direccion -> Direccion -> Bool
iguales    Norte        Norte     = True
iguales    Este         Este      = True
iguales    Sur          Sur       = True
iguales    Oeste        Oeste     = True
iguales    _            _        = False

siguiente :: Direccion -> Direccion
siguiente    Norte      = Este
siguiente    Este       = Sur
siguiente    Sur        = Oeste
siguiente    Oeste      = error "La direccion Oeste no tiene siguiente" 

-- La funcion tiene como precondicion que Direccion no puede ser Oeste y esta misma es parcial ya que no todos sus casos tiene el resultado esperado.

-- 2)

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia =  (Lunes, Domingo) 


empiezaConM :: DiaDeSemana -> Bool
empiezaConM    Martes      = True
empiezaConM    Miercoles   = True
empiezaConM    _           = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues     d              d2       =  numeroDelDia d > numeroDelDia d2

numeroDelDia :: DiaDeSemana -> Int  
numeroDelDia    Lunes       =  1
numeroDelDia    Martes      =  2
numeroDelDia    Miercoles   =  3
numeroDelDia    Jueves      =  4
numeroDelDia    Viernes     =  5
numeroDelDia    Sabado      =  6
numeroDelDia    Domingo     =  7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio    Lunes       =  False
estaEnElMedio    Domingo     =  False
estaEnElMedio    _           =  True

-- 3)

negar :: Bool -> Bool
negar   True  = False
negar   False = True

implica :: Bool -> Bool -> Bool
implica    True    b    = b 
implica    False   _    = True 

yTambien :: Bool -> Bool -> Bool
yTambien    True    b    = b
yTambien    False   _    = False

oBien :: Bool -> Bool -> Bool
oBien    False   b    =  b
oBien    True    _    = True

{- EJERCIO 4 Registros -}

-- 1)

data Persona = P String Int
--               nombre edad
    deriving Show

joaco = (P "Joaco" 23)
nahue = (P "Nahue" 21)

nombre :: Persona -> String 
nombre    (P nombre edad) =  nombre

edad :: Persona -> Int
edad    (P nombre edad) =  edad

crecer :: Persona -> Persona
crecer    (P nombre edad) = (P nombre (edad + 1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre    nuevoNombre (P nombre edad) = (P nuevoNombre edad)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra    p           p2     = edad p > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor    p           p2      = if(edad p > edad p2)
                                        then p
                                      else p2

-- 2)

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = Pokemon TipoDePokemon Int
    deriving Show

data Entrenador = Entrenador String Pokemon Pokemon 
    deriving Show

charmander = (Pokemon Fuego 80)
bulbasaur  = (Pokemon Planta 80)
magikarp   = (Pokemon Agua 50)

entrenador1 = (Entrenador "Joaco" charmander bulbasaur)
entrenador2 = (Entrenador "Nahue" magikarp charmander)

superaA :: Pokemon -> Pokemon -> Bool
superaA     p1          p2    = esMejorTipo   (tipoDe p1) (tipoDe p2) 

tipoDe :: Pokemon -> TipoDePokemon
tipoDe    (Pokemon tipo energia) = tipo

esMejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMejorTipo    Agua             Fuego         = True 
esMejorTipo    Fuego            Planta        = True 
esMejorTipo    Planta           Agua          = True 
esMejorTipo    _                _             = False 

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe    tipo             (Entrenador nombre p1 p2) = unoSiEsYCeroSiNo (sonDelMismoTipo tipo (tipoDe p1)) + unoSiEsYCeroSiNo (sonDelMismoTipo tipo (tipoDe p2))    

sonDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipo    Agua             Agua          = True
sonDelMismoTipo    Fuego            Fuego         = True
sonDelMismoTipo    Planta           Planta        = True
sonDelMismoTipo    _                _             = False

unoSiEsYCeroSiNo :: Bool -> Int
unoSiEsYCeroSiNo    True  =  1
unoSiEsYCeroSiNo    False =  0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon     (entrenador1, entrenador2) = (pokemonesDe entrenador1) ++ (pokemonesDe entrenador2)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe    (Entrenador nombre p1 p2) = [p1, p2]

{- EJERCIO 5 Funciones polimorficas -}

-- 1) 

loMismo :: a -> a 
loMismo    x =  x

siempreSiete :: a -> Int
siempreSiete    x =  7

swap :: (a, b) -> (b, a)
swap    (x, y) =  (y, x)

-- Estas funciones son polimorficas porque lo que importa es su estructura y no su información.

-- 2)

estaVacia :: [a] -> Bool
estaVacia    []  =  True
estaVacia    _ =  False  

elPrimero :: [a] -> a 
elPrimero    (x : _) = x   

sinElPrimero :: [a] -> [a]
sinElPrimero    (_ : y) = y  

splitHead :: [a] -> (a, [a])
splitHead    (x : ys) = (x, ys)