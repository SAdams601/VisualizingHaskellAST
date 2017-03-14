module Explodes2 where
import qualified Data.DList as D

ex0 :: Int -> [a] -> D.DList a
ex0 n lst = D.fromList (concat (map (\x -> replicate n x) lst))

ex1 :: Int -> [a] -> D.DList a
ex1 n lst = (D.fromList (concat (map (\x -> D.toList (D.replicate n x)) lst)))

ex2 :: Int -> [a] -> D.DList a
ex2 n lst = D.fromList (concat (D.map (\x -> replicate n x) (D.fromList lst)))

ex3 :: Int -> [Int] -> D.DList Int
ex3 n lst = D.fromList (concat (D.map (\x -> D.toList (D.replicate n x)) (D.fromList lst)))
         
ex4 :: Int -> [a] -> D.DList a
ex4 n lst = D.concat (map (\x -> D.fromList (replicate n x)) lst)

ex5 :: Int -> [a] -> D.DList a
ex5 n lst = D.concat (map (\x -> D.replicate n x) lst)

ex6 :: Int -> [a] -> D.DList a
ex6 n lst = D.concat (D.toList (D.map (\x -> D.fromList (replicate n x)) (D.fromList lst)))

ex7 :: Int -> [a] -> D.DList a
ex7 n lst = D.concat (D.toList (D.map (\x -> D.replicate n x) (D.fromList lst)))

