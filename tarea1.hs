-- Tarea 1
-- Grupo 11:
-- Roberto Omaña 06-39990
-- Leopoldo Pimentel 06-40095

--1.
nPrimos :: Int -> Int -> [Int]
nPrimos n m = [ x | x<-[n..m], esPrimo x]

esPrimo :: Int -> Bool
esPrimo n = [ x | x<-[1..n], mod n x == 0] == [1,n]

--2.
emparejar :: [a] -> [b] -> [(a,b)]
emparejar = emparejarCon (,)

emparejarCon :: (a->b->c) -> [a]->[b]->[c]
emparejarCon z (x:xs) (y:ys) = z x y : emparejarCon z xs ys
emparejarCon _ _ _ =  []

--3.
desemparejar :: [(a,b)] -> ([a],[b])
desemparejar []  =  ([], [])
desemparejar xs = (,) (desemparejarAux fst xs) (desemparejarAux snd xs)

desemparejarAux :: (a->b)->[a]->[b]
desemparejarAux _ [] = []
desemparejarAux f (x:xs) = f x : desemparejarAux f xs

--4.
--zip y unzip, respectivamente.

--5.
separarMitad :: [a] -> ([a],[a])
separarMitad xs
     | mod longitud 2 == 0 = (take (div longitud 2) xs, drop (longitud - div longitud 2) xs)
     | otherwise = (take (div longitud 2) xs, drop (longitud - div longitud 2 - 1) xs)
          where longitud = length xs
                
--6.
mergeSort :: Ord a => [a] -> [a]
mergeSort []    =  []
mergeSort [x]  =  [x]
mergeSort xs   =  mezcla (mergeSort ladoizquierdo) (mergeSort ladoderecho)
  where
    (ladoizquierdo, ladoderecho) = separarMitad xs
    mezcla [] xs = xs
    mezcla xs [] = xs
    mezcla (x:xs) (y:ys)
      | x <= y      = x : mezcla xs (y:ys)
      | otherwise = y : mezcla (x:xs) ys