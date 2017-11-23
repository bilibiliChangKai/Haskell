module Main where

-- 插入排序
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n > x = x : insert n xs
                | otherwise = n : (x:xs)

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insert x $ insertSort xs

-- 冒泡排序
bubbo :: Ord a => a -> [a] -> [a]
bubbo x [] = [x]
bubbo x (y:ys) | x > y = y : bubbo x ys
               | otherwise = x : bubbo y ys

bubboSort :: Ord a => [a] -> [a]
bubboSort [] = []
bubboSort [x] = [x]
bubboSort (x:xs) = (bubboSort bxs) ++ [bx]
                   where (bxs, bx) = (init bub, last bub)
                         bub = bubbo x xs 

-- 选择排序
delete :: Eq a => a -> [a] -> [a]
delete x [] = [x]
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort xs = minx : selectSort remainxs
              where minx = minimum xs
                    remainxs = delete minx xs

-- 快速排序
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort minxs ++ [x] ++ quickSort maxxs
                 where minxs = filter (<x) xs
                       maxxs = filter (>=x) xs

main = putStrLn "Hello World"