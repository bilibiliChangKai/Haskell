module Main where

import System.IO
import           CheckCreditNumber

-- 问题1
t1 :: IO ()
t1 = putStrLn "t1"
t2 :: IO String
t2 = getLine
t3 :: String -> IO String
t3 = return
t4 :: String -> IO ()
t4 = putStrLn
t5 :: IO String -> IO ()
t5 ios = do
  s <- ios
  putStrLn s
t6 :: IO String -> IO String
t6 ios = do
  s <- ios
  return $ s ++ "t6"
t7 :: [IO String] -> IO ()
t7 [] = return ()
t7 (ios:left) = do
  s <- ios
  putStrLn s
  t7 left
t8 :: Show a => a -> IO ()
t8 = print
t9 :: a -> IO a
t9 = return

-- 读取全部卡号
readCards :: Handle -> IO [Integer]
readCards fp = do
  str <- hGetContents fp
  return $ fmap read $ lines str

-- 排序并除去重复卡号
sortCards :: [Integer] -> [Integer]
sortCards = undefined

-- 检查卡号并存到文件中
checkCards :: [Integer] -> IO ()
checkCards [] = return ()
checkCards (c:cs) = do
  fpv <- openFile "validCard.txt" WriteMode
  fpi <- openFile "invalidCard.txt" WriteMode
  if isValid c
    then hPrint fpv c
    else hPrint fpi c

main :: IO ()
main = do
  fp <- openFile "cards200.txt" ReadMode
  cards <- readCards fp
  checkCards $ sortCards cards
