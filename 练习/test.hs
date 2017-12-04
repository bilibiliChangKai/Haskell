module Main where

import System.Random

guess :: IO ()
guess = do
  r <- randomIO :: IO Int
  let n = mod r 101
  guessNum n

guessNum :: Int -> IO ()
guessNum n = do
  putStrLn "Please input a number:"
  s <- getInt
  if s == n
    then putStrLn "You guess the true number!"
    else if s > n
      then do
        putStrLn "The right number is small than you guess!"
        guessNum n
      else do
        putStrLn "The right number is big than you guess!"
        guessNum n

getInt :: IO Int
getInt = do
  a <- getLine
  return (read a)

ttt :: IO a
ttt = undefined

main :: IO ()
main = putStrLn "Hello World"
