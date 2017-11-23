module Boolprop
    (
    ) where

-- Prop
data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  deriving Eq

-- Show Prop 实现
instance Show Prop where
  show n1 = let f n2 = tupleNum n2 == Two in
   case n1 of
     Const b     -> show b
     Var c       -> [c]
     Not p       -> '~':addBracket (\n3 -> tupleNum n3 /= Zero) p
     And p1 p2   -> addBracket f p1 ++ " && " ++ addBracket f p2
     Or p1 p2    -> addBracket f p1 ++ " || " ++ addBracket f p2
     Imply p1 p2 -> addBracket f p1 ++ " => " ++ addBracket f p2

-- 通过给定的函数判断需不需要加括号
addBracket :: (Prop -> Bool) -> Prop -> String
addBracket f n = if f n
  then "(" ++ show n ++ ")"
  else show n

-- 符号元组数定义
-- 数字和符号为零元祖
-- ~为一元组
-- &&, ||, =>为二元组
data Tuple = Two | One | Zero deriving (Eq)

-- Prop 优先级
tupleNum :: Prop -> Tuple
tupleNum p = case p of
  Const _ -> Zero
  Var _   -> Zero
  Not _   -> One
  _       -> Two

p1, p2, p3 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Or (Var 'A') (Not (Var 'A'))
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
