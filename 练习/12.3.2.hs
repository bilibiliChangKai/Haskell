module Main where

import Control.Monad.Trans.State
import Data.Functor.Identity

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

peek :: State Stack Int
peek = state $ \(x:xs) -> (x, x:xs)

push :: Int -> State Stack Int
push x = state $ \xs -> (x, x:xs)

addStack :: State Stack Int
addStack = do
        a1 <- pop
        a2 <- pop
        push (a1 + a2)

main = putStrLn "Hello World"