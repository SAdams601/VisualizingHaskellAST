module Type where

type Foo = (String, Int)

getInt :: Foo -> Int
getInt (_,i) = i


