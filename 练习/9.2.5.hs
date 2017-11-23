module Main where

import Control.Applicative
import Data.Char

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

-- runParser :: Parser a -> String -> Maybe (a, String)
instance Functor Parser where
    fmap f p = Parser $ \str -> case runParser p str of
                                Just (a, s) -> Just (f a, s)
                                _ -> Nothing

instance Applicative Parser where
    pure a = Parser $ \str -> Just (a, str)
    (<*>) pf pa = Parser $ \str -> case runParser pf str of
                                Nothing -> Nothing
                                Just (f, s) -> case runParser pa s of
                                        Just (a, s1) -> Just (f a, s1)
                                        _ -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (<|>) pa pb = Parser $ \str -> case runParser pa str of
                                Nothing -> runParser pb str
                                just -> just

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \str -> case str of
                                [] -> Nothing
                                s:ss -> if f s then Just (s, ss) else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

------------------------
number :: Parser Int
number = fmap (foldl (\x y -> 10 * x + y) 0) (many digit)
    where digit = fmap digitToInt (satisfy isDigit)

sque :: Parser a -> Parser [a] -> Parser [a]
sque x y = Parser $ \str -> case runParser x str of 
                            Nothing -> Nothing
                            Just (s, ss) -> case  runParser y ss of 
                                            Nothing -> Nothing
                                            Just (s1, ss1) -> Just (s:s1, ss1)

parseStr :: [Char] -> Parser [Char]
parseStr strs = foldr sque (Parser $ \str -> Just ("", str)) [char s | s <- strs]


main = putStrLn "Hello World"