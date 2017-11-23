module Main where

import Control.Monad.Trans.Writer
import Data.Monoid
import Control.Monad
import Data.Char

left, right :: Int -> Writer String Int
left x = writer (x - 1, "move left\n")
right x = writer (x + 1, "move right\n")

move i = do
        x <- left i
        y <- listen (right x)
        return y

------

-- 合并操作
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
        | x <= y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys
-- 类似tab符
indent :: Int -> ShowS
indent n = showString (take (4 * n) (repeat ' '))

-- 回车\n
nl :: ShowS
nl = showChar '\n'

-- 总排序函数
mergesort :: Int -> [Int] -> Writer String [Int]
mergesort l [] = return []
mergesort l s@[x] = return [x]
mergesort l s@xs = do
                tell $ (indent l.showString "mergesort: ".shows s.nl) ""
                let (a1, a2) = splitAt (length s `div` 2) xs
                tell $ (indent (l + 1).showString "merge".shows a1.shows a2.nl) ""
                liftM2 merge (mergesort (1 + 2) a1) (mergesort (1 + 2) a2)

main = print "helloworld"