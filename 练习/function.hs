module Main where
import Prelude hiding ((!!))

(!!) :: [a] -> Int -> a
(!!) list num = head $ drop num list

month :: Int -> Int
month = undefined

days :: (Int, Int) -> Int
days (m, d) = month m + d

cons = (:)

sons :: a -> [a] -> [a]
sons x [] = [x]
sons x (y : ys) = y : (sons x ys)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) = if a == x then True else elem' a xs 

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete a (x : xs) = if a == x then delete a xs else x : delete a xs

drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' _ [] = []
drop' a (x : xs) = drop' (a - 1) xs

main = putStrLn "Hello World"