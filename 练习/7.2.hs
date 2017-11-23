
module Main where 
--import Prelude hiding ((++))
--import Prelude hiding ((>>))
import Prelude hiding (elem, notelem)

snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

test :: [a] -> [a] -> [a]
test = foldr snoc

-- (++) :: [a] -> [a] -> [a]
-- (++) = foldr snoc

-- _concat :: [[a]] -> [a]
-- _concat = foldr (++) [] 

---------------------------

_map :: (a -> b) -> [a] -> [b]
_map f = foldr (\l ls -> f l : ls) []

{-????-}
_unwords :: [String] -> String
_unwords [] = ""
_unwords strs = foldr1 (\w s -> w ++ " " ++ s) strs

---------------------------

_maximum, _minimum :: Ord a => [a] -> a
_maximum = foldr1 max
_minimum = foldr1 min

_filter :: (a -> Bool) -> [a] -> [a]
_filter f list = foldr (\x xs -> if f x then x : xs else xs) [] list

-- (>>) :: (a -> b) -> (b -> c) -> (a -> c)
-- (>>) = flip (.)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

elem, notelem :: Eq a => a -> [a] -> Bool
elem x ls = any (==x) ls
notelem x ls = all (/=x) ls

main = putStrLn "Hello World"