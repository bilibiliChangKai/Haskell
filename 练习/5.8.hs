module Main where

search :: Ord a => a -> [a] -> Bool
search _ [] = False
search a xs | m < a = search a behind
            | m > a = search a front
            | otherwise = True
                where (front, m : behind) = splitAt (length xs `div` 2) xs


main = putStrLn "Hello World"