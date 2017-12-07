module Game where

import System.Random

-- 定义简单石头布类型
data Hand = Rock | Scissor | Paper deriving (Enum)
instance Random Hand where
    random g = case randomR (0,2) g of
                     (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
            (r, g') -> (toEnum r, g')

play :: IO ()
play = undefined
