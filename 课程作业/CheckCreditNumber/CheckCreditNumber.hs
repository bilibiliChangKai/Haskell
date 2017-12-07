module CheckCreditNumber where
import           Data.List

numValid :: [Integer] -> Integer
numValid xs = sum . map (const 1) $ filter isValid xs

isValid :: Integer -> Bool
isValid = mod10
    .sum
    .divideInteger
    .doubleEver
    .divideIntegerAndReverse

divideIntegerAndReverse :: Integer -> [Integer]
divideIntegerAndReverse 0 = []
divideIntegerAndReverse x = (x `mod` 10) : divideIntegerAndReverse (x `div` 10)

doubleEver :: [Integer] -> [Integer]
doubleEver []       = []
doubleEver [x]      = [x]
doubleEver (x:y:xs) = [x, 2 * y] ++ doubleEver xs

divideInteger :: [Integer] -> [Integer]
divideInteger [] = []
divideInteger (x:xs)
    | x > 9 = divideIntegerAndReverse x ++ divideInteger xs
    | otherwise = x : divideInteger xs

mod10 :: Integer -> Bool
mod10 x = mod x 10 == 0

checkCreditNumber = putStrLn "Hello World"
