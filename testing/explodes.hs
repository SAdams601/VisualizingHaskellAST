module Explodes where
import qualified Data.DList as D


explode :: Int -> [a] -> [a]
explode n lst = concat (map (\x -> replicate n x) lst)

ex0 :: Int -> D.DList a -> [a]
ex0 n lst = concat (map (\x -> replicate n x) (D.toList lst))

ex1 :: Int -> D.DList a -> [a]
ex1 n lst = concat (map (\x -> D.toList (D.replicate n x)) (D.toList lst))

ex2 :: Int -> D.DList a -> [a]
ex2 n lst = concat (D.map (\x -> replicate n x) lst)

ex3 :: Int -> D.DList a -> [a]
ex3 n lst = concat (D.map (\x -> (D.toList (D.replicate n x))) lst)

ex4 :: Int -> D.DList a -> [a]
ex4 n lst = D.toList (D.concat (map (\x -> D.fromList (replicate n x)) (D.toList lst)))
        
ex5 :: Int -> D.DList a -> [a]
ex5 n lst = D.toList (D.concat (map (\x -> D.replicate n x) (D.toList lst)))

ex6 :: Int -> D.DList a -> [a]
ex6 n lst = D.toList (D.concat (D.toList (D.map (\x -> D.fromList (replicate n x)) lst)))

ex7 :: Int -> D.DList a -> [a]
ex7 n lst = D.toList (D.concat (D.toList (D.map (\x -> D.replicate n x) lst)))

