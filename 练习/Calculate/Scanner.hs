module Scanner where
import Data.Char
import Calculate

-- string -> [LitOp]
scanExp :: String -> [LitOp]
scanExp [] = error "Excepted an expression"
scanExp (' ':xs) = scanExp xs
-- 单元运算符
scanExp ('l':'o':'g':xs) = Right Log : scanExp xs
scanExp ('s':'i':'n':xs) = Right Sin : scanExp xs
scanExp ('c':'o':'s':xs) = Right Cos : scanExp xs
scanExp ('s':'q':'r':'t':xs) = Right Sqrt : scanExp xs
scanExp ('+':xs) = Right Posi : scanExp xs
scanExp ('-':xs) = Right Nega : scanExp xs
scanExp ('(':xs) = Right L_Par : scanExp xs
-- 其他情况
scanExp xs = scanNum xs

scanNum :: String -> [LitOp]
scanNum ('e':xs) = Left (Const "e") : scanBin xs
scanNum ('p':'i':xs) = Left (Const "pi") : scanBin xs
scanNum xs = let (num, rest) = span isDigit xs in
    if null num then error "Excepted a number or constant" 
        else case rest of 
                ('.':r) -> let (float, r') = span isDigit r in
                    Left (Val (read (num ++ "." ++ float)::Float)) : scanBin r'
                r -> Left (Val (read num::Float)) : scanBin r

-- 二元运算符后面是一个完整的表达式
-- 将右括号归在二元运算符里面，但是右括号后面还需要一个二元运算符
scanBin :: String -> [LitOp]
scanBin [] = []
scanBin (' ':xs) = scanBin xs
scanBin ('+':xs) = Right Plus : scanExp xs
scanBin ('-':xs) = Right Minu : scanExp xs
scanBin ('*':xs) = Right Mult : scanExp xs
scanBin ('/':xs) = Right Divi : scanExp xs
scanBin ('^':xs) = Right Power : scanExp xs
scanBin (')':xs) = Right R_Par : scanBin xs
scanBin _ = error "Excepted an infix binary operator"