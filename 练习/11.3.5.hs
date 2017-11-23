{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO

class Printf t where
    printf :: String -> t

format :: Show t => String -> t -> String
format ('%' : 's' : cs) cs' = show cs' ++ cs
format (c : cs) cs' = c : format cs cs'
format "" cs' = ""

ttt :: a -> a
ttt = forall

instance Printf (IO ()) where
    printf cs = putStrLn cs

instance (Show u, Printf t) => Printf (u -> t) where
    printf cs = \x -> printf (format cs x)

test1 :: IO ()
test1 = printf "%s and %s are friends." "hu" "hou"

main = putStrLn "Hello World"