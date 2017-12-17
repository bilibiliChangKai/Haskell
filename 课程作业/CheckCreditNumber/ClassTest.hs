module Main where

import           CheckCreditNumber
import           Data.List
import           System.IO

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
  return $ read <$> lines str

-- 排序并除去重复卡号
formatCards :: [Integer] -> [Integer]
formatCards = deleteRepeatCards.sort

-- 除去重复卡号
deleteRepeatCards :: [Integer] -> [Integer]
deleteRepeatCards [] = []
deleteRepeatCards [c] = [c]
deleteRepeatCards (c1:c2:cs) = if c1 == c2
  then deleteRepeatCards (c2:cs)
  else c1:deleteRepeatCards (c2:cs)

-- 检查卡号并存到文件中
checkCards :: [Integer] -> IO ()
checkCards [] = return ()
checkCards cs = do
  fpv <- openFile "validCard.txt" WriteMode
  fpi <- openFile "invalidCard.txt" WriteMode
  writeCards fpv fpi cs
  hClose fpv
  hClose fpi

-- 写文件
writeCards :: Handle -> Handle -> [Integer] -> IO ()
writeCards _ _ [] = return ()
writeCards v i (c:cs) = do
  if isValid c
    then hPrint v c
    else hPrint i c
  writeCards v i cs

main :: IO ()
main = do
  fp <- openFile "cards200.txt" ReadMode
  cards <- readCards fp
  checkCards $ formatCards cards
  putStrLn "Done"
