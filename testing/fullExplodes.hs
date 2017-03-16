module FullExplodes where
import qualified Data.DList as D

ex0 :: Int -> D.DList a -> D.DList a
ex0 n lst = D.fromList (concat (map (\x -> replicate n x) (D.toList lst)))


ex1 :: Int -> D.DList Int -> D.DList Int
ex1 n lst = D.fromList (concat (map (\x -> D.toList (D.replicate n x)) (D.toList lst)))

ex2 :: Int -> D.DList a -> D.DList a
ex2 n lst = D.fromList (concat (D.toList (D.map (\x -> replicate n x) lst)))

ex3 :: Int -> D.DList a -> D.DList a
ex3 n lst = D.concat (map (\x -> D.fromList (replicate n x)) (D.toList lst))

{-
ex0 :: Int -> D.DList a -> D.DList a
ex0 n lst = D.fromList (concat (map (\x -> replicate n x) (D.toList lst)))

ex0 :: Int -> D.DList a -> D.DList a
ex0 n lst = D.fromList (concat (map (\x -> replicate n x) (D.toList lst)))

ex0 :: Int -> D.DList a -> D.DList a
ex0 n lst = D.fromList (concat (map (\x -> replicate n x) (D.toList lst)))

ex0 :: Int -> D.DList a -> D.DList a
ex0 n lst = D.fromList (concat (map (\x -> replicate n x) (D.toList lst)))
-}
