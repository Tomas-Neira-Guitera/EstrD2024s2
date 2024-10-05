module Empresa 

    (ConsE, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)

    where 

    type SectorId = Int

    type CUIL = Int
 
                    --    Map 1                         Map 2
    data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

    -- INV REP: * Si el Map 2 es vacio, los Set del Map 1 estan vacios.
    --          * Un empleado puede estar en el Map 2 y no estar en el Map 1, pero no puede estar en el Map 1 y no estar en el Map 2.
    --          * Si un empleado no se encuentra en el Map 2, no se encuentra en el Map 1.
    --          * Por cada empleado del Set del Map 1, SectorId se debe encontrar dentro de "sectores Empleado". 

    -- Map 1: relaciona los sectores con los empleados que trabajan ahi.
    -- Map 2: relaciona un CUIL a un Empleado.

    -- Empleado es un TAD. El empleado 44851926 puede estar en el sector 3 y 4.
    -- El empleado tiene un arbol con los sectores donde trabaja.

    -- En los costos S son la cantidad de sectores y E son la cantidad de empleados.

    consEmpresa :: Empresa  -- Costo O(1)
    consEmpresa = ConsE emptyM emptyM

    buscarPorCUIL :: CUIL -> Empresa -> Empleado  -- Costo: O(log E)
    -- precondicion: el cuil existe en la empresa
    buscarPorCUIL    cuil    (ConsE mapSE mapCE) = fromJust (lookupM cuil mapCE)

    empleadosDelSector :: SectorId -> Empresa -> [Empleado]  -- Costo: O(log S + E)
    empleadosDelSector    unSector    (ConsE mapSE mapCE) = case lookupM unSector mapSE of
                                                                Nothing -> []
                                                                Just s -> setToList s

    todosLosCUIL :: Empresa -> [CUIL]   -- Costo: O(E)
    todosLosCUIL    (ConsE mapSE mapCE) = keys mapCE


    todosLosSectores :: Empresa -> [SectorId]  -- Costo: O(S)
    todosLosSectores    (ConsE mapSE mapCE) = keys mapSE


    agregarSector :: SectorId -> Empresa -> Empresa   -- Costo: O(log S)
    agregarSector    unSector    (ConsE mapSE mapCE) = case lookupM unSector mapSE of
                                                        Nothing -> ConsE (assocM unSector emptyS mapSE) mapCE
                                                        Just e  -> ConsE mapSE mapCE
                                                        

    agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
    agregarEmpleado    []            cuil    empresa             = empresa 
    agregarEmpleado    (s:ss)        cuil    (ConsE mapSE mapCE) = agregarEmpleadoS s cuil (agregarEmpleado ss cuil (ConsE mapSE mapCE))

    agregarEmpleadoS :: SectorId -> CUIL -> Empresa -> Empresa
    agregarEmpleadoS    s           cuil    (ConsE mapSE mapCE) = case lookupM s mapSE of
                                                                    Nothing        -> ConsE mapSE mapCE
                                                                    Just empleados -> agregarASector s cuil (ConsE (assocM s (addS (conEmpleado cuil) empleados) mapSE) mapCE)
    
    agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
    agregarASector    s           cuil    (ConsE mapSE mapCE) = case lookupM cuil mapCE of 
                                                                    Nothing       -> ConsE mapSE mapCE
                                                                    Just empleado -> ConsE mapSE (assocM cuil (incorporarSector s empleado) mapCE)
                                                                    
    -- Observacion: no me estoy asegurando de que SectorId este en Empresa.

    borrarEmpleado :: CUIL -> Empresa -> Empresa
    borrarEmpleado    cuil    (ConsE mapSE mapCE) = ConsE (borrarEmpleadoM cuil mapSE (keys mapSE)) (deleteM cuil mapCE)

    borrarEmpleadoM :: CUIL -> Map SectorId (Set Empleado) -> [SectorId] -> Map SectorId (Set Empleado)
    borrarEmpleado     cuil    map                            []         = map
    borrarEmpleado     cuil    map                            (s:ss)     = assocM s (removeS (conEmpleado cuil) (fromJust lookupM s map)) (borrarEmpleadoM cuil map ss)


    