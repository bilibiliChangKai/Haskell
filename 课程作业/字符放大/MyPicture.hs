module MyPicture where
import Data.Char
import Data.List
import System.IO
import System.IO.Unsafe

type Picture = [String]

dictionaryName :: FilePath
dictionaryName = "Data\\"    

-- 加载文件中的01字符串，并格式化后返回
getPicture :: Char -> IO Picture
getPicture c
    | isLower c = getPicture $ toUpper c
    | isUpper c || isDigit c = do
        f <- openFile (dictionaryName ++ (c:".txt")) ReadMode
        content <- hGetContents f
        return $ formatPicture c $ lines content
    | otherwise = error "getPicture error: input char is not digit or letter!"

-- 将Picture中的01替换成空格和字符c
formatPicture :: Char -> Picture -> Picture
formatPicture c ls = fmap (formatString c) ls

formatString :: Char -> String -> String
formatString _ [] = []
formatString c (x:xs) = case x of
    '0' -> ' ':formatString c xs
    '1' -> c:formatString c xs
    _ -> error "formatString error: has the character not equal '0' or '1'" 

-- 依次将字符串中的字符转换成Picture，并进行拼接
sayToPicture :: String -> IO Picture
sayToPicture [] = return []
sayToPicture (x:xs) = do
    p1 <- getPicture x
    other <- sayToPicture xs
    return $ p1 +++ other

-- 由于使用了文件管理函数
-- 因此无法实现String->String的纯函数类型
-- 只能退求其次变成现在这样
say :: String -> IO String
say [] = error "say error: It is couldn't empty!"
say s = do
    ls <- sayToPicture s
    return $ unlines ls

sayit :: String -> IO ()
sayit [] = putStr "You don't input anything!"
sayit s = do
    out <- say s
    putStr out

-- 两个Picture的拼接，中间加上三个空格
infix 6 +++
(+++) :: Picture -> Picture -> Picture
(+++) ls [] = ls
(+++) [] ls = ls
(+++) ls1 ls2 = [x ++ "   " ++ y | (x, y) <- zip ls1 ls2]

test :: Char -> Picture
test = unsafePerformIO.getPicture

myPicture = putStrLn "Hello World"