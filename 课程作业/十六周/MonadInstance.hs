module Main (main) where

import Data.Monoid

-- Monad
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just a) = Just $ f a
-- (<$) = fmap const

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> ma = fmap f ma
-- pure (const id) :: f (a -> b -> b)
-- pure (const id) <*> u :: f (b -> b)
-- u *> v = pure (const id) <*> u <*> v
-- u <* v = pure const <*> u <*> v

instance Monad Maybe where
  return = pure
  Nothing >>= _ = Nothing
  (Just a) >>= f = f a

-- IO
instance Functor IO where
  fmap f ma = do
    a <- ma
    case a of
      () -> return ()
      _  -> return $ f a

instance Applicative IO where
  pure = return
  mf <*> ma = do
    f <- mf
    case f of
      () -> return ()
      _  -> fmap f ma

instance Monad IO where
  return = return
  ma >>= f = do
    a <- ma
    case a of
      () -> return ()
      _  -> f a

-- []
instance Functor [] where
  fmap = map

instance Applicative [] where
  pure a = [a]
  mf <*> ma = [f a | f <- mf, a <- ma]

instance Monad [] where
  return = pure
  ma >>= f = concatMap f ma

-- (,) a
instance Functor ((,) a) where
  fmap f (c, a) = (c, f a)

instance (Monoid a) => Applicative ((,) a) where
  pure a = (mempty, a)
  (m1, f) <*> (m2, a) = (m1 `mappend` m2, f a)

instance (Monoid a) => Monad ((,) a) where
  return = pure
  (m1, a) >>= f = let (m2, b) = f a in
    (m1 `mappend` m2, b)

-- (->) a
instance Functor ((->) m) where
  fmap = (.)

instance (Monoid m) => Applicative ((->) m) where
  pure = const
  (<*>) fmab fma m = fmab m (fma m)

instance (Monoid m) => Monad ((->) m) where
  return = pure
  (>>=) fma famb m = famb (fma m)

main :: IO ()
main = undefined
