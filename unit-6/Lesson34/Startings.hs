module Main where

-- lsiting 34.2
head :: Monoid a => [a] -> a
head (x:xs) = x
head [] = mempty 

-- listing 34.4.
example :: [[Int]]
example = []

