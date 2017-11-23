module PrintTicket where
import Text.Printf
import Test.QuickCheck
-- 15331111 胡子昂

type Items = [Item]
type Item = (Name, Amount, Price)
type Name = String -- 物品姓名
type Amount = Float -- 物品数量
type Price = Float  -- 单个物品价格

-- 一个测试数据
testData :: Items
testData = [
    ("AppleTTTTTT", 2.5, 8.8),
    ("Banana", 1.2, 7.989),        
    ("Milk", 6, 1.134)
    ]

-----------------  实现部分 --------------------
-- 输出清单
printTicket :: Items -> IO ()
printTicket [] = putStrLn "You not buy anything!"
printTicket is = do
            printHead
            printItems fis
            printTail fis
            where fis = formatItems is

-- 打印清单头
printHead :: IO ()
printHead = putStrLn $ formatStr "Name" ++ formatStr "Amount"
                    ++ formatStr "Price" ++ formatStr "Sum"

-- 打印清单尾
printTail :: Items -> IO ()
printTail [] = error "No Item!"
printTail is = putStrLn $ formatStr "Total" ++ take 14 (repeat '.')
                        ++ take 6 (repeat ' ') ++ formatFloat (calculateTotal is)


-- 输出Items
printItems :: Items -> IO () 
printItems [] = return ()
printItems (i:is) = do
                printItem i
                printItems is

-- 输出Item   
printItem :: Item -> IO ()
printItem (n, 0, 0) = putStrLn $ formatStr n
printItem (n, a, p) = putStrLn $ formatStr n ++ formatFloat a 
                            ++ formatFloat p ++ formatFloat (a * p)

-- 计算总价格
calculateTotal :: Items -> Float
calculateTotal [] = 0
calculateTotal ((n, a, p):is) = a * p + calculateTotal is

-- 检查Item.name，如果name长度大于7，分割成两组
-- 另一组name加上-用于标识连在一起，amount和price都为0
formatItems :: Items -> Items
formatItems [] = []
formatItems (i@(n, a, p):is) = case length n > 7 of
                            True -> (take 7 n, 0, 0) 
                                    : formatItems (("-" ++ drop 7 n, a, p) : is)
                            _ -> i : formatItems is

-- 规范String和Float的格式
formatStr :: String -> String
formatStr = printf "%-10s"
formatFloat :: Float -> String
formatFloat = printf "%-10.2f"

------------------ 测试部分 ---------------
-- 出现的问题：我发现@只能从简到繁，而反过来是不行的
-- 例如：i@(n, a, p) -> True
--       (n, a, p)@i -> False

-- 测试是否对齐
prop_formatStr :: String -> Property
prop_formatStr s = let l = length s in
            l < 10 ==> formatStr s == s ++ take (10 - l) (repeat ' ')

-- 测试只有两个item的计算结果
prop_calculateTotal :: Item -> Item -> Bool
prop_calculateTotal i1@(n1, a1, p1) i2@(n2, a2, p2) = 
                        abs (a1 * p1 + a2 * p2 - calculateTotal [i1, i2]) < 0.00001