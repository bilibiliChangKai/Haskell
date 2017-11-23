module Main where

-- romeNotation :: [String]
-- romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

-- romeAmount :: [Int]
-- romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

-- pair :: [(Int, String)]
-- pair = zip romeAmount romeNotation

-- pairs :: [(String, Int)]
-- pairs = zip romeNotation romeAmount

-- subtrahead :: Int -> (Int, String)
-- subtrahead n = head (dropWhile (\(a, _) -> a > n) pair)

-- covert :: Int -> (Int, String)
-- covert 0 = (0, "")
-- covert n = let (i, st) = subtrahead n in 
--           let (i2, st2) = covert (n - i) in (i2, st ++ st2)

-- -- 表示第一个是子串的值
-- subinthead :: String -> (String, Int)
-- subinthead str = head (dropWhile (\(sub, _) -> not $ substr sub str) pairs)

-- -- 反转换
-- uncovert :: String -> (String, Int)
-- uncovert "" = ("", 0)
-- uncovert str = let (sub, value) = subinthead str in
--                let (subless, valueless) = uncovert (drop (length sub) str) in (subless, value + valueless)

-- -- 字串查找
-- substr :: String -> String -> Bool
-- substr "" _ = True
-- substr _ "" = False
-- substr sub str = if head sub == head str then substr (drop 1 sub) (drop 1 str) else False

getRemain :: Int -> String
getRemain n | n < 0 = error "don't less than 0"
            | odd n = "1"
            | otherwise = "0"


tenconvertTwo :: Int -> String
tenconvertTwo 0 = ""
tenconvertTwo n = (tenconvertTwo $ div n 2) ++ (getRemain n)

convert :: Int -> String
convert n | n == 0 = "0"
          | otherwise = tenconvertTwo n

main = putStrLn "Hello World"