module Main where

data MyMaybe a = MyNothing | MyJust a deriving Show

instance Functor MyMaybe where
    fmap f MyNothing = MyNothing
    fmap f (MyJust a) = MyJust (f a)

instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    (MyJust f) <*> arg = fmap f arg

instance Alternative MyMaybe where
    empty = MyNothing
    MyNothing <|> p = p
    (MyJust x) <|> _ = MyJust x

test :: Num a => a -> a -> a -> a
test a1 a2 a3 = a1 + a2 + a3

----------------------
{--
instance Applicative ((->) r) where
    -- pure :: a -> (r -> a)
    -- ==> a -> r -> a
    pure x _ = x 
    -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
    -- ==> (r -> a -> b) -> (r -> a) -> r -> b
    -- 还有这种操作
    (<*>) f g x = f x (g x)
--}

main = putStrLn "Hello World"