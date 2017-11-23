module Main where
import Data.List

fibo :: (Eq a, Num a) => a -> a
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- fiboStep :: Num a => (a, a) -> (a, a)
-- fiboStep (n, v) = (v, n + v)

-- fiboPair :: (Eq a, Num a) => a -> (a, a)
-- fiboPair 0 = (0, 1)
-- fiboPair n = fiboStep (fiboPair (n - 1))

-- fastFib :: (Eq a, Num a) => a -> a
-- fastFib n = fst $ fiboPair n

fib :: (Eq a, Num a) => a -> a -> a -> a
fib 0 f1 f2 = f2
fib n f1 f2 = fib (n - 1) f2 (f1 + f2)
fibos n = fib (n - 1) 0 1

fiboss n = map fibos [1..n]

-- golden :: Fractional a => Int -> [a]
-- golden n = map (\(x, y) -> x / y) (fiboss n)

main = putStrLn "Hello World"