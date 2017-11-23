module Main where

hanoi :: Int -> [(Int, Int)]
hanoi n = move (n, 1, 2, 3)

-- from开始地点，to结束地点，via中间的位置
move :: (Int, Int, Int, Int) -> [(Int, Int)]
move (1, form, to, via) = [(form, to)]
move (n, form, to, via) = move (n - 1, form, via, to) ++ [(form, to)]
                          ++ move (n - 1, via, to, form)

main = putStrLn "Hello World"