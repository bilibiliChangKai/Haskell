module Main where

data MyNum = One | Two | Three

instance Show MyNum where
    show One = "1"
    show Two = "2"
    show Three = "3"


main = putStrLn "Hello World"