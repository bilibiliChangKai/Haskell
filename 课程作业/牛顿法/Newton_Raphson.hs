module Newton_Raphson where
import Test.QuickCheck

-- 给定一个数x，返回迭代n次后的值
squareroot2 :: Float -> Integer -> Float
squareroot2 x 0 = x
squareroot2 x n 
    | x == 0 = error "x could't be 0"
    | n < 0 = error "N is less then 0"
    | otherwise = let x' = (x + 2 / x) / 2 in
                    squareroot2 x' (n - 1)

-- 给定任意值r，从x0开始，返回迭代n次后的值
squareroot :: Float -> Float -> Integer -> Float
squareroot _ x 0 = x
squareroot r x n
    | x == 0 = error "x could't be 0"
    | r < 0 = error "r is less then 0"
    | n < 0 = error "N is less then 0"
    | otherwise = let x' = (x + r / x) / 2 in
                    squareroot r x' (n - 1)

-- 返回squareroot产生的x0..xn序列
sqrtSeq :: Float -> Float -> [Float]
sqrtSeq r x
    | x == 0 = error "x could't be 0"
    | r < 0 = error "r is less then 0"
    | otherwise = let x' = (x + r / x) / 2 in
                    x : sqrtSeq r x'

-- 类似squareroot，但是次数无限，直到误差值小于e为止
squareroot' :: Float -> Float -> Float -> Float
squareroot' r x e
    | x == 0 = error "x could't be 0"
    | e < 0 = error "e is less then 0"
    | r < 0 = error "r is less then 0"
    | otherwise = let sequ = sqrtSeq r x in
                     snd $ head $ dropWhile (\(x, y) -> abs (x - y) >= e) $ zip sequ (tail sequ)

----------------------------------------- 测试部分
-- squareroot2 测试结果是否为sqrt 2(x 不能和2差距太大)
prop_squareroot2 :: Float -> Property
prop_squareroot2 x = x /= 0 && abs x < 1000 
                ==> abs (sqrt 2 - (abs $ squareroot2 x 500)) <= 0.00001

-- squareroot 测试结果是否为sqrt r(x r 差距不能太大)
prop_squareroot :: Float -> Float -> Property
prop_squareroot r x = x /= 0 && abs x < 1000 && r > 0 && r < 1000
                ==> abs (sqrt r - (abs $ squareroot r x 500)) <= 0.00001

-- sqrtSeq 测试当x > r > 1 时，数组是否从大到小排序
-- 辅助函数，判断数组是否从大到小排序
isLessOrder :: [Float] -> Bool
isLessOrder [] = True
isLessOrder [x] = True
isLessOrder (x:y:xs)
    | x >= y = isLessOrder (y:xs)
    | otherwise = False

prop_sqrtSeq :: Float -> Float -> Property
prop_sqrtSeq r x = x > r && r > 1 ==> isLessOrder $ take 10 $ sqrtSeq r x

-- squareroot' 类似squareroot的测试方法，但是误差e可以自己选定
prop_squareroot' :: Float -> Float -> Float -> Property
prop_squareroot' r x e = x /= 0 && r > 0 && e > 0
                ==> abs (sqrt r - (abs $ squareroot' r x e)) <= e

newton_Raphson = putStrLn "Hello World"