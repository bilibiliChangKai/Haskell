{-# LANGUAGE GADTs #-}

module Main where

import Data.List (insertBy, sortBy, permutations)
import Data.Ord (comparing)

-- 卡斯兰数
{-
data Tree = Leaf | Node Tree Tree deriving Show

intToTrees :: Int -> [Tree]
intToTrees 0 = [Leaf]
intToTrees n = [Node lt rt | l <- [0..(n - 1)], lt <- intToTrees l, rt <- intToTrees (n - 1 - l)]

brace :: Tree -> String
brace Leaf = ""
brace (Node l r) = '(':brace l ++ ")" ++ brace r
-}

-- 哈夫曼树
{-
data HTree a where
    Leaf :: (Char, Float) -> HTree a  
    Node :: HTree a -> HTree a -> HTree a
    deriving (Eq, Show)

haffmanTree :: [(Char, Float)] -> HTree a
haffmanTree x = addTree $ pairsToNodes x

addTree :: [HTree a] -> HTree a
addTree [] = Leaf ('n', 0)
addTree [p] = p
addTree (x1:x2:hpairs) = addTree (insert (Node x1 x2) hpairs)

pairsToNodes :: [(Char, Float)] -> [HTree a]
pairsToNodes [] = []
pairsToNodes (p:pairs) = insert (Leaf p) (pairsToNodes pairs)

insert :: HTree a -> [HTree a] -> [HTree a]
insert p [] = [p]
insert p hpairs = insertBy (comparing value) p hpairs

value :: HTree a -> Float
value (Leaf a) = snd a
value (Node l r) = value l + value r

showHTree :: HTree a -> [(Char, String)]
showHTree (Leaf p) = [(fst p, "")]
showHTree (Node l r) = [(c, '0':s)|(c, s) <- showHTree l] ++ [(c, '1':s)|(c, s) <- showHTree r]
-}

-- Zipper
{-
data Zipper a = Zipper [a] a [a] deriving (Eq, Show)

listToZipper :: [a] -> Zipper a
listToZipper (x:xs) = Zipper [] x xs
listToZipper _ = error "Empty!"

next :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y:ys) x xs
next z = z

prev :: Zipper a -> Zipper a
prev (Zipper (y:ys) x xs) = Zipper ys y (x:xs)
prev z = z
-}

-- 树类型Zipper
{-
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)
data Accumulate a = Empty | R (Accumulate a) a (Tree a)
                          | L (Accumulate a) a (Tree a)
type Zipper a = (Tree a, Accumulate a)

right, left, up :: Zipper a -> Zipper a
right (Node n l r, a) = (r, R a n l)
right z = z

left (Node n l r, a) = (l, L a n r)
left z = z

up (r, R a n l) = (Node n l r, a)
up (l, L a n r) = (Node n l r, a)
up z = z
-}

-- 解24点
data Exp where
    Val :: Double -> Exp
    Plus :: Exp -> Exp -> Exp
    Sub :: Exp -> Exp -> Exp
    Mult :: Exp -> Exp -> Exp
    Div :: Exp -> Exp -> Exp
    deriving (Show)

instance Eq Exp where
    a == b = showExp a == showExp b

eval :: Exp -> Double
eval (Val a) = a
eval (Plus a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a / eval b

showExp :: Exp -> String
showExp (Val a) = show a
showExp (Plus a b) = showExp a ++ "+" ++ showExp b
showExp (Sub a b) = showExp a ++ "-" ++ showExp b
showExp (Mult a b) = l ++ "*" ++ r
                    where l = addDash (calValue a < 2) a
                          r = addDash (calValue b < 2) b
showExp (Div a b) = l ++ "/" ++ r
                    where l = addDash (calValue a < 2) a
                          r = addDash (calValue b < 2) b

calValue :: Exp -> Int
calValue (Plus a b) = 1
calValue (Sub a b) = 1
calValue (Mult a b) = 2
calValue (Div a b) = 2
calValue _ = 10

addDash :: Bool -> Exp -> String
addDash True e = "(" ++ showExp e ++ ")"
addDash False e = showExp e

divideList :: [a] -> [([a], [a])]
divideList l = [(take n l, drop n l) | n<-[1..(length l - 1)]]

buildExpressions :: ([Exp], [Exp]) -> [Exp]
buildExpressions (el1, el2) = [op e1 e2 | e1<-el1, e2<-el2, op<-[Plus, Sub, Mult, Div]]

toExpressions :: [Double] -> [Exp]
toExpressions [] = []
toExpressions [x] = [Val x]
toExpressions xs = concat [buildExpressions (toExpressions l, toExpressions r) | (l, r)<-divideList xs]

generate :: [Double] -> [Exp]
generate ns =  concatMap toExpressions $ permutations ns

reflesh :: Eq a => [a] -> [a]
reflesh [] = []
reflesh (a:as) | elem a as = reflesh as
               | otherwise = a : reflesh as

twentyfour :: [Double] -> [String]
twentyfour ns = [showExp x | x<-generate ns, eval x == 24.0]

main = putStrLn "Hello World"