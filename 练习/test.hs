module Main where

import System.Random
import Control.Monad.Trans.State

ttt :: a -> b -> (a -> b)
ttt _ y _ = y

abc :: Num b => (b -> b)
abc = ttt 1 2

main :: IO ()
main = putStrLn "hello"
