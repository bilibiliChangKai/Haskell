module Main where

data Pair a b = Pair a b deriving (Show)
pfirst (Pair a b) = a
psecond (Pair a b) = b

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun  deriving (Eq, Show, Ord, Enum)

type Name = String
type Author = String
type Price = Float

data Book = Book {
    name   ::   Name,
    author ::   Author,
    price  ::   Price 
} deriving (Eq, Show)

test :: ([Int], [Int]) -> Int -> ([Int], [Int])
test (is1, is2) i = (flip (:) is1 i, flip (:) is2 i)

incresePrice :: ([Book], [Book]) -> Book -> Price -> ([Book], [Book])
incresePrice (bs1, bs2) b pri = 
    (flip (:) bs1 b, flip (:) bs2 (Book (name b) (author b) (price b + pri)))

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

disjoint :: [a] -> [b] -> [Either a b]
disjoint as bs = map Left as ++ map Right bs

_either :: (a -> c) -> (b -> c) -> Either a b -> c
_either f _ (Left x) = f x
_either _ g (Right y) = g y

-- 8.14

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToint :: Nat -> Int
natToint Zero = 0
natToint (Succ n) = 1 + natToint n

intTonat :: Int -> Nat
intTonat 0 = Zero
intTonat n = Succ (intTonat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- 8.15
data Shape = Circle {
    radius :: Float
} | Rect {
    len :: Float,
    width :: Float
}  deriving (Eq, Show)

zhouchang :: Shape -> Float
zhouchang (Circle r) = 2 * 3.14 * r
zhouchang (Rect l w) = 2 * (l + w)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

listToMyList :: [a] -> List a
listToMyList [] = Nil
listToMyList (x:xs) = Cons x $ listToMyList xs

myListToList :: List a -> [a]
myListToList Nil = []
myListToList (Cons x ls) = x : myListToList ls


main = putStrLn "Hello World"