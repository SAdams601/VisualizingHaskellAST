module S where

s :: [Int] -> Int
s lst = foldr (+) 0 lst

exponents :: Int -> [Int]
exponents base = base : (exponents (2*base))

addtwo :: [Int] -> [Int]
addtwo lst = map (2+) lst
