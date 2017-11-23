module Main where

data Tree a = Leaf a | Branch (Tree (a, a)) deriving Show

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    --fmap f (Tree a) = Tree (fmap f a)
    fmap f (Branch a) = Branch (fmap (f2 f) a)
    --fmap f (Branch (Tree (t1, t2))) = undefined

f2 :: (a -> b) -> (a, a) -> (b, b)
f2 f (a1, a2) = (f a1, f a2)

test :: Integer -> Integer
test i = i + 1

t = Branch (Branch (Leaf ((1, 2),(3, 4))))

main = putStrLn "Hello World"