module Main where
import Control.Monad.Trans.State

test :: [Int] -> [Int]
test xs = [(+1) x | x<-xs, even x]

main = putStrLn "Hello World"