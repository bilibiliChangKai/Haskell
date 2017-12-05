module Guess where

import System.Random
import Control.Monad.Trans.State

guess :: String
guess = "QiaoBangZhu"

tryGuess :: IO String
tryGuess = do
  putStr "请输入字符串："
  getLine

hideGuessStr :: String
hideGuessStr = take (length guess) $ repeat '-'

initState :: IO State String ()
initState = return $ state $ \_ -> ((), hideGuessStr)

getState :: IO State String String
getState = return $ state $ \s -> (s, s)

-- 更新State
updateState :: String -> IO State String ()
updateState s1 = return $ state $ \s2 -> ((), updateString s1 s2 guess)

-- 更新String，第一个String是输入字符，第二个String是储存字符
-- 第三个String是正确字符
updateString :: String -> String -> String  -> String
updateString [] _ _ = []
updateString _ [] _ = []
updateString (c1:s1) (c2:s2) (c3:s3) = case c2 of
  '-' -> if c1 == c3
    then c3:leftString
    else '-':leftString
  otherwise -> c2:leftString
  where leftString = updateString s1 s2 s3


main :: IO ()
main = do
  sta <- initState
  putStrLn $ (++) "The guess string is " $ runState sta []
  yourStr <- tryGuess
