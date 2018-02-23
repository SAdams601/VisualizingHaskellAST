module Test where

cn :: [[a]] -> [a]
cn lst = foldl (++) [] lst
