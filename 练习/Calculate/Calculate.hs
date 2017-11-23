module Calculate where

import Control.Monad.Trans.State

-- Val Float代表常数，Const String代表特殊数字比如"e"，"pi"
data Lit = Val Float | Const String | Empty deriving (Eq, Show)

-- 运算符，OpBottom表示栈底
data Op = Posi | Nega 
    | Plus | Minu | Mult | Divi
    | Power | Log | Ln
    | Sin | Cos | Sqrt
    | L_Par | R_Par
    | OpBottom  deriving (Eq, Show)

-- 运算符所需数字定义
data Order = Unary | Binary | Null | Bottom

-- 运算符需要几个元素
nary :: Op -> Order
nary op = case op of
        Plus -> Binary
        Minu -> Binary
        Mult -> Binary
        Divi -> Binary
        Power -> Binary
        Posi -> Unary
        Nega -> Unary
        Log -> Unary
        Ln -> Unary
        Sin -> Unary
        Cos -> Unary
        Sqrt -> Unary
        OpBottom -> Bottom
        _ -> Null

-- 运算符的优先级
priority :: Op -> Int
priority op = case op of
            Plus -> 1
            Minu -> 1
            Mult -> 2
            Divi -> 2
            Log -> 3
            Sin -> 3
            Cos -> 3
            Posi -> 4
            Nega -> 4
            Sqrt -> 4
            Power -> 5
            _ -> 0

type LitOp = Either Lit Op
type Stack = ([LitOp], [LitOp])

-- 根据表达式和输入的数，进行计算并将计算结果压入栈
evaluate :: Op -> LitOp -> LitOp -> State Stack ()
evaluate op (Left (Val f1)) (Left (Val f2))
                = push.lv $ case op of
                    Plus -> f1 + f2
                    Minu -> f1 - f2
                    Mult -> f1 * f2
                    Divi -> f1 / f2
                    Power -> f1 ** f2
evaluate op (Left (Val f1)) (Left Empty)
                = push.lv $ case op of
                    Posi -> f1
                    Nega -> -f1
                    Log -> logBase 2 f1
                    Ln -> log f1
                    Sin -> sin f1
                    Cos -> cos f1
                    Sqrt -> sqrt f1

-- 辅助函数，类似于Monad
lv :: Float -> LitOp
lv x = Left $ Val x

-- 左栈出和右栈出
pop0, pop1 :: State Stack LitOp
pop0 = state $ \(ls, rs) -> case ls of
    [] -> error "Number stack underflow"
    (h:hs) -> (h, (hs, rs))
pop1 = state $ \(ls, rs) -> case rs of
    [] -> error "Number stack underflow"
    (h:hs) -> (h, (ls, hs))
-- 入栈函数
push :: LitOp -> State Stack ()
push (Left (Const "pi")) = push.lv $ 3.1415926
push (Left (Const "e")) = push.lv $ 2.7182812
push (Left (Const _)) = error "The const is not exist!"
push l@(Left _) = state $ \(ls, rs) -> ((), (l:ls, rs))
push r@(Right _) = state $ \(ls, rs) -> ((), (ls, r:rs))

pushIn :: LitOp -> State Stack ()
pushIn l@(Left num) = push l
pushIn r@(Right L_Par) = push r
pushIn r@(Right R_Par) = do
    Right top <- pop1
    case nary top of
        Null -> return ()
        Unary -> do
            f1 <- pop0
            evaluate top f1 (Left Empty)
            pushIn r
        Binary -> do
            f1 <- pop0
            f2 <- pop0
            evaluate top f2 f1
            pushIn r
        Bottom -> error "Excepted Left Bracket"
pushIn o@(Right op) = do
    case nary op of
        Unary -> push o
        Binary -> do
            Right top <- pop1
            case nary top of
                Unary -> do
                    let pri = priority top > priority op
                    case pri of
                        True -> do
                            f1 <- pop0
                            evaluate top f1 (Left Empty)
                            pushIn o
                        False -> do
                            push (Right top)
                            push o
                Binary -> do
                    case op of
                        Power -> do
                            push (Right top)
                            push o
                        _ -> do 
                            let pri = priority top >= priority op
                            case pri of
                                True -> do
                                    f1 <- pop0
                                    f2 <- pop0
                                    evaluate top f2 f1
                                    pushIn o
                                False -> do
                                    push (Right top)
                                    push o
                _ -> do -- L_Par and OpBottom
                    push (Right top)
                    push o

-- 计算LitOp数组中的表达式
calc :: [LitOp] -> State Stack LitOp
calc [] = do
    Right op <- pop1
    case nary op of
        Bottom -> pop0
        Unary -> do
            f1 <- pop0
            evaluate op f1 (Left Empty)
            calc []
        Binary -> do
            f1 <- pop0
            f2 <- pop0
            evaluate op f2 f1
            calc []
        Null -> error "Excepted right bracket"
calc (t:ts) = do
    pushIn t
    calc ts

-- 初始化
inits :: ([LitOp], [LitOp])
inits = ([], [Right OpBottom])

-------------- Test ---------------
{-
testFunc :: [LitOp] -> Integer -> State Stack ()
testFunc _ 0 = state $ \lr -> ((), lr)
testFunc (t:ts) n = do
    pushIn t
    testFunc ts (n - 1)
-}

test1 = [Right Nega, Left (Const "pi")]

-- (5+6)*3
test2 = [Right L_Par, Left (Val 5.0), Right Plus, Left (Val 6), Right R_Par, Right Mult, Left (Val 3.0)]

calculate = putStrLn "Hello World"