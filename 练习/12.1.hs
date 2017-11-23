module Main where

import Control.Monad.Trans.Writer
import Data.Monoid
import Control.Monad

-- 并操作
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 缩进，等于tab键
indent :: Int -> ShowS
indent n = showString (take (4 * n) (repeat ' '))

nl :: ShowS
nl = showChar '\n'

-- 缩进数 -> 要排序数组 -> 排序过程
mergeSort :: Int -> [Int] -> Writer String [Int]
mergeSort l [] = do
                return []
mergeSort l s@[x] = do
                return [x]
mergeSort l s@xs = do
                tell $ (indent l.showString "mergesort: ".shows s.nl) ""
                let (a1, a2) = splitAt (length s `div` 2) xs
                tell $ (indent (l + 1).showString "merge".shows a1.shows a2.nl) ""
                liftM2 merge (mergeSort (l + 2) a1) (mergeSort (l + 2) a2)

main = putStrLn "Hello World"