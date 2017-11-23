{-
    胡子昂，15331111，3254266353@qq.com，数据科学与计算机系，软件工程专业
    其他说明：
        测试函数已全部通过。
        输入分数时，分母必须>0。
-}

module MyFraction where
import Test.QuickCheck

type Fraction = (Integer, Integer)

------------------- 函数实现部分 ----------------
-- 使二元组中的a>=b
aBigThanb :: (Integer, Integer) -> (Integer, Integer)
aBigThanb (a, b) = if a >= b then (a, b) else (b, a)

-- 用辗转相除法，求最大公因数，忽略负号
maxCommonDivisor :: Integer -> Integer -> Integer
maxCommonDivisor x y = let (a, b) = aBigThanb (abs x, abs y) in
                       let rem = a `mod` b in
                    case rem of
                        0 -> b
                        otherwise -> maxCommonDivisor b rem

-- 用于化简
simply :: Fraction -> Fraction
simply (0, y) = (0, 1)
simply (x, y) = let maxdivisor = maxCommonDivisor x y in
                (x `div` maxdivisor, y `div` maxdivisor)

-- 加减乘除实现
ratplus, ratminus, rattimes, ratdiv :: Fraction -> Fraction -> Fraction
ratplus (a, b) (c, d) = simply (a * d + b * c, b * d)
ratminus (a, b) (c, d) = simply (a * d - b * c, b * d)
rattimes (a, b) (c, d) = simply (a * c, b * d)
-- 确保相除后分母>0
ratdiv _ (0, _) = error "Can not divided by zero!"
ratdiv (0, y) _ = (0, y)
ratdiv (a, b) (c, d) = if c > 0
                    then simply (a * d, b * c)
                    else simply (a * (-d), b * (-c))

-- 向下取整
ratfloor :: Fraction -> Integer
ratfloor (x, y) = x `div` y

-- 转换成浮点型
ratfloat :: Fraction -> Float
ratfloat (x, y) = (fromInteger x) / (fromInteger y)

-- 判断是否相等
rateq :: Fraction -> Fraction -> Bool
rateq (0, _) (0, _) = True
rateq (a, b) (c, d) = let (a', b') = simply (a, b) in
                      let (c', d') = simply (c, d) in
                    a' == c' && b' == d'

-- 四个符号实现
infix 5 <+>, <->
(<+>), (<->) :: Fraction -> Fraction -> Fraction
(<+>) x y = ratplus x y
(<->) x y = ratminus x y

infix 6 <-*->, </>
(<-*->) :: Fraction -> Fraction -> Fraction
(<-*->) x y = rattimes x y
(</>) x y = ratdiv x y

--------------------- 测试部分 -------------------------
-- rateq
-- 测试分子分母各相乘一个固定值后是否相等
prop_rateq :: Fraction -> Integer -> Property
prop_rateq (a, b) mul = b > 0 && mul > 0 ==> (a, b) `rateq` (a * mul, b * mul)

-- simply
-- 测试任何分母为1的数化简后不变
prop_simply1 :: Integer -> Bool
prop_simply1 x = simply (x, 1) == (x, 1)

-- 测试分数化简后转成float的值不变
prop_simply2 :: Fraction -> Property
prop_simply2 (a, b) = b > 0 ==> (ratfloat (a, b)) == (ratfloat $ simply (a, b))

-- ratplus
-- 测试分数加一个常数add等同于分子加上add*分母
prop_ratplus1 :: Fraction -> Integer -> Property
prop_ratplus1 (a, b) add = b > 0 ==> ((a, b) <+> (add, 1)) `rateq` (a + add * b, b)

-- ratminus
-- 同上
prop_ratminus1 :: Fraction -> Integer -> Property
prop_ratminus1 (a, b) add = b > 0 ==> ((a, b) <-> (add, 1)) `rateq` (a - add * b, b)

-- 测试x+y == x-(-y) 
prop_ratminus2 :: Fraction -> Fraction -> Property
prop_ratminus2 (a, b) (c, d) = b > 0 && d > 0 ==> ((a, b) <+> (c, d)) `rateq` ((a, b) <-> (-c, d))

-- rattimes
-- 测试分数*常数
prop_rattimes1 :: Fraction -> Integer -> Property
prop_rattimes1 (a, b) mul = b > 0 ==> ((a, b) <-*-> (mul, 1)) `rateq` (a * mul, b)

-- 测试分数*自己的倒数=1
prop_rattimes2 :: Fraction -> Property
prop_rattimes2 (a, b) = b > 0 && a /= 0 ==> if a > 0
                                        then ((a, b) <-*-> (b, a)) `rateq` (1, 1)
                                        else ((a, b) <-*-> (-b, -a)) `rateq` (1, 1)

-- ratdiv
-- 测试分数/常数 = 分母*常数
prop_ratdiv1 :: Fraction -> Integer -> Property
prop_ratdiv1 (a, b) div = b > 0 && div /= 0 ==> if div > 0
                                            then ((a, b) </> (div, 1)) `rateq` (a, b * div)
                                            else ((a, b) </> (div, 1)) `rateq` (-a, -(b * div))

-- 测试某数/某数 = 1
prop_ratdiv2 :: Fraction -> Property
prop_ratdiv2 (a, b) = b > 0 && a /= 0 ==> ((a, b) </> (a, b)) `rateq` (1, 1)

-- ratfloor
-- 测试x+1/x -> 1 or -1
prop_ratfloor :: Integer -> Property
prop_ratfloor x = x > 1 || x < -1 ==> if x > 1
                                    then ratfloor (x + 1, x) == 1
                                    else ratfloor (x + 1, -x) == -1

myFraction = putStrLn "Hello World"