module Set2
  
  (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
  
  where

  data Set a = S [a] 

  --Crea un conjunto vacío.
  emptyS :: Set a
  emptyS = (S [])
  
  
  --Dados un elemento y un conjunto, agrega el elemento al conjunto.
  addS :: Eq a => a -> Set a -> Set a
  addS a (S xs) = (S (xs ++ [a])) 
  
  --Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
  belongs :: Eq a => a -> Set a -> Bool
  belongs a (S xs) = elem a xs
  
  
  --Devuelve la cantidad de elementos distintos de un conjunto.
  sizeS :: Eq a => Set a -> Int
  sizeS (S xs) = length (sinRepetidos xs)
  
  sinRepetidos ::Eq a => [a] -> [a]
  sinRepetidos []       = []
  sinRepetidos (x : xs) = eliminoRepetidos x xs 
  
  eliminoRepetidos :: Eq a => a -> [a] -> [a]
  eliminoRepetidos x []       = [x]
  eliminoRepetidos x (y : ys) = if (x == y)
                                then eliminoRepetidos y ys
                                else x : (eliminoRepetidos y ys)
  
  
  
  removeS :: Eq a => a -> Set a -> Set a
  removeS a (S []) = (S [])
  removeS a (S xs) = if elem a xs
                      then (S (deleteElem a xs))
                      else (S xs)
  
  deleteElem :: Eq a => a -> [a] -> [a]
  deleteElem a (x:xs) = if (a == x)
                        then xs
                        else x : (deleteElem a xs)
  
  
  
  unionS :: Eq a => Set a -> Set a -> Set a
  unionS (S xs)  (S ys)  = (S (consolidarSetV2s (sinRepetidos xs) (sinRepetidos ys)))
  
  
  consolidarSetV2s :: Eq a => [a] -> [a] -> [a]
  consolidarSetV2s xs        [] = xs
  consolidarSetV2s []        ys = ys
  consolidarSetV2s (x : xs)  ys = if (elem x ys)
                                then consolidarSetV2s xs ys
                                else x : (consolidarSetV2s xs ys)
  
  --Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
  setToList :: Eq a => SetV2 a -> [a]
  setToList (S xs)= sinRepetidos xs
  