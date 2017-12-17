module Main where

import System.Random
import Control.Monad.Trans.State

guess :: String
guess = "QiaoBangZhu"

tryGuess :: IO String
tryGuess = do
  putStr "请输入字符串："
  getLine

-- 更新String，第一个String是输入字符，第二个String是储存字符
-- 第三个String是正确字符
updateString :: String -> String -> String  -> String
updateString [] _ _ = []
updateString _ [] _ = []
updateString (c1:s1) (c2:s2) (c3:s3) = case c2 of
  '-' -> if c1 == c3
    then c3:leftString
    else '-':leftString
  otherwise -> c2:leftString
  where leftString = updateString s1 s2 s3

fibs :: [Integer]
fibs = 1:1:[x + y | (x, y) <- zip fibs (tail fibs)]

main :: IO ()
main = do
  putStrLn "hello"
