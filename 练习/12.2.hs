module Main where

--import Control.Monad.Trans.Reader
import Data.Monoid
import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }

reader = Reader

instance Functor (Reader r) where
    fmap f ra = Reader $ \r -> let a = runReader ra r in
                                f a

instance Applicative (Reader r) where
    pure a = Reader $ \_ -> a
    rf <*> ra = Reader $ \r -> let f = runReader rf r in
                               let a = runReader ra r in
                                f a

instance Monad (Reader r) where
    return a = pure a
    m >>= f = Reader $ \r -> runReader (f (runReader m r)) r

readLen :: Reader [a] Int
readLen = reader $ \r -> length r

readHead :: Reader [a] a
readHead = reader $ \(x:xs) -> x

main = putStrLn "Hello World"