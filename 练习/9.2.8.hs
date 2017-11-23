module Main where

import Data.Foldable
import Data.Monoid

data Tree a = Leaf a | Node (Tree a) a (Tree a)  deriving (Eq, Show)

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l n r) = foldMap f l `mappend` f n `mappend` foldMap f r

flatten :: Tree a -> [a]
flatten = foldMap (: [])

main = putStrLn "Hello World"