module Nat where

import           Test.QuickCheck

-- Nat类型
data Nat = Succ Nat | Zero deriving (Eq)

-- Nat的Show类实现
instance Show Nat where
  show n = show $ nat2Int n

-- 转换函数
int2Nat :: Int -> Nat
int2Nat i
  | i < 0 = error "Input int can't less 0"
  | i == 0 = Zero
  | otherwise = Succ (int2Nat $ i - 1)
nat2Int :: Nat -> Int
nat2Int Zero     = 0
nat2Int (Succ n) = 1 + nat2Int n

add :: Nat -> Nat -> Nat
add n1 n2 = int2Nat $ nat2Int n1 + nat2Int n2

-- t1 = int2Nat 3, t2 = int2Nat 7
t1, t2 :: Nat
t1 = Succ (Succ (Succ Zero))
t2 = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

-- 测试部分
prop_natInt1 :: Int -> Property
prop_natInt1 n = n >= 0 ==> n == (nat2Int $ int2Nat n)

prop_natInt2 :: Int -> Property
prop_natInt2 n = n >= 0 ==> int2Nat (n + 1) == (Succ (int2Nat n))

nat :: IO ()
nat = putStrLn "Hello World"
