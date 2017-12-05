module Boolprop
    (
    Prop(..),
    p1, p2, p3, p4, p5, p6,  -- :: Prop, 用于测试
    eval,  -- :: Subst -> Prop -> Bool, 执行Prop并得到结果
    vars,  -- :: Prop -> [Char], 得到Prop中所有占位字符
    substs,  -- :: Prop -> [Subst], 得到Prop中所有占位字符的全部可能
    isTaut  -- :: Prop -> Bool, 判断Prop是否为永真
    ) where

-- Prop
data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  deriving Eq

-- 符号元组数定义
-- 数字和符号为零元祖
-- ~为一元组
-- &&, ||, =>为二元组
data Tuple = Two | One | Zero deriving (Eq)

-- 真值带入
type Subst = [(Char, Bool)]

-- 步骤一:
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

-- Prop 优先级
tupleNum :: Prop -> Tuple
tupleNum p = case p of
  Const _ -> Zero
  Var _   -> Zero
  Not _   -> One
  _       -> Two

-- 步骤二
-- 用于测试
p1, p2, p3, p4, p5, p6 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Or (Var 'A') (Not (Var 'A'))
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Var 'A'
p5 = And (Var 'P') (Var 'Q')
p6 = Imply (Var 'B') (Or (Var 'A') (Var 'B'))

-- 步骤三:
-- 执行Prop并得到结果
eval :: Subst -> Prop -> Bool
eval subs n1 = case n1 of
    Const b     -> b
    Var c       -> findBoolbySubst subs c
    Not p1      -> not $ eval subs p1
    And p1 p2   -> eval subs p1 && eval subs p2
    Or p1 p2    -> eval subs p1 || eval subs p2
    Imply p1 p2 -> not (eval subs p1) || eval subs p2

-- 从列表中找到对应x的y值
findBoolbySubst :: Subst -> Char -> Bool
findBoolbySubst [] c = error $ "Subst not exit" ++ [c]
findBoolbySubst ((c1,b):subs) c2 = if c1 == c2
  then b
  else findBoolbySubst subs c2

-- 步骤四:
-- 得到Prop中所有占位字符
vars :: Prop -> [Char]
vars (Const b)     = []
vars (Var c)       = [c]
vars (Not p)       = vars p
vars (And p1 p2)   = delRepeat $ vars p1 ++ vars p2
vars (Or p1 p2)    = delRepeat $ vars p1 ++ vars p2
vars (Imply p1 p2) = delRepeat $ vars p1 ++ vars p2

-- 删除重复元素
delRepeat :: [Char] -> [Char]
delRepeat [] = []
delRepeat (c:cs) = if c `elem` cs
  then delRepeat cs
  else c:delRepeat cs

-- 得到Prop中所有占位字符的全部可能
substs :: Prop -> [Subst]
substs = charsToSubsts.vars

-- 辅助函数,chars -> substs
charsToSubsts :: [Char] -> [Subst]
charsToSubsts [] = []
charsToSubsts [c] = [[(c, True)], [(c, False)]]
charsToSubsts (c:cs) = let leftSubsts = charsToSubsts cs in
  fmap (\subs -> (c, True):subs) leftSubsts ++ fmap (\subs -> (c, False):subs) leftSubsts

-- 步骤五:
-- 判断Prop是否为永真
-- 将subst p中的所有可能性全部eval组成一个Bool列表
-- 若列表不存在False,则为全真
isTaut :: Prop -> Bool
isTaut p = case substs p of
  []        -> eval [] p
  otherwise -> False `notElem` fmap (`eval` p) (substs p)
