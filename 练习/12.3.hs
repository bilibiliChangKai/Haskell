module Main where

import Control.Monad.Trans.State
import Data.Functor.Identity

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

labelTree :: Tree a -> Tree (a, Int)
labelTree t = evalState (ntAux t) 0

increase :: State Int Int
increase = state $ \t -> (t, t + 1)

ntAux :: Tree a -> State Int (Tree (a, Int))
ntAux (Leaf a) = do
                num <- increase
                return $ Leaf (a, num)
ntAux (Node l a r) = do
                    num <- increase
                    ln <- ntAux l
                    rn <- ntAux r
                    return $ Node ln (a, num) rn

test :: Tree Int
test = Node (Node (Leaf 5) 3 (Leaf 2)) 7 (Leaf 9)

main = putStrLn "Hello World"