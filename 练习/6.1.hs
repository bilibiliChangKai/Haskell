module Main where

series2 :: Int -> [Double]
series2 n = [1 / (2 * (fromIntegral k) + 1) * (-1) ^ k * (1 / 2) ^ (2 * k + 1) | k <- [0..n]]

series3 :: Int -> [Double]
series3 n = [1 / (2 * (fromIntegral k) + 1) * (-1) ^ k * (1 / 3) ^ (2 * k + 1) | k <- [0..n]]

main = putStrLn "Hello World"