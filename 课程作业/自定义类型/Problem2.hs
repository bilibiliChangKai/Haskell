module Exp where

-- 符号优先级定义,由于只有+,-,*,仅需两个,None代表数字
data Prop = High | Low | None deriving (Eq)

-- Exp定义
data Exp = Lit Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp

instance Show Exp where
  show = exp2String

prop :: Exp -> Prop
prop (Mul _ _) = High
prop (Lit _)   = None
prop _         = Low

-- 通过函数判断是否给字符串加入括号
addBracket :: (Exp -> Bool) -> Exp -> String
addBracket f n = if f n
  then "(" ++ show n ++ ")"
  else show n

-- Exp 转换成 String
exp2String :: Exp -> String
exp2String (Lit i)     = show i
exp2String (Add n1 n2) = show n1 ++ '+':show n2
exp2String (Sub n1 n2) = show n1 ++ '-':show n2
exp2String (Mul n1 n2) = addBracket (\n -> prop n == Low) n1
  ++ '*':addBracket (\n -> prop n == Low) n2

-- 测试样例
t1, t2, t3, t4, t5, t6 :: Exp
t1 = Lit 2
t2 = Mul (Lit 2) (Add (Lit 3) (Lit 5))
t3 = Add (Mul (Lit 2) (Lit 3)) (Lit 5)
t4 = Mul (Add (Lit 2) (Lit 3)) (Lit 5)
t5 = Sub (Lit 2) (Add (Lit 3) (Lit 5))
t6 = Mul (Lit 2) (Mul (Lit 3) (Lit 5))

exp :: IO ()
exp = putStrLn "Hello World"
