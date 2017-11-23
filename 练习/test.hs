module Main where

func :: Integer -> Integer
func x = (+) 1 $ case x == 0 of 
            True -> 1
            _ -> 0

divisors :: Integer -> [Integer]
divisors value = [x | x <- [1..value], value `mod` x == 0]

-- 返回可以整除n的数，该数范围在0-n
divs :: Integer -> Integer -> [Integer]
divs _ 0 = []
divs n d = if n `mod` d == 0 
            then d : divs n (d - 1)
            else divs n (d - 1)

triands :: Integer -> [(Integer, Integer, Integer)]
triands value = [(x, y, z) | x<-[1..value], y<-[1..value], z <- [1..value], x*x + y*y == z*z]

main = putStrLn "Hello World"