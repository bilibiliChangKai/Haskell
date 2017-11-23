module Main where

--import Control.Monad.Trans.Writer
import Data.Monoid
import Control.Monad

newtype Writer w a = Writer { runWriter :: (a, w) }

--版本1
instance Functor (Writer w) where
    fmap f (Writer (x, v)) = let y = f x in
                            Writer (y, v)

instance (Monoid w) => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    (Writer (f, v)) <*> (Writer (x, v2)) = let y = f x
                                        in Writer (y, v2 `mappend` v)

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v2)) = f x 
                         in Writer (y, v `mappend` v2)

{-
--版本2
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let xv = (x, v) in
                            let (Writer (y, v2)) = f xv 
                         in Writer (y, v `mappend` v2)
-}

main = putStrLn "Hello World"