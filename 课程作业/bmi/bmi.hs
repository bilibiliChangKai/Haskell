module Bmi where
import           Text.Printf

getFloat :: IO Float
getFloat = do
  str <- getLine
  return $ read str

calBMI :: Float -> Float -> Float
calBMI h w = w / (h * h)

showStatue :: Float -> String
showStatue b
  | b < 0     = error "invalid input"
  | b < 18.5  = "体重过低"
  | b < 24    = "体重正常"
  | b < 28    = "超重"
  | otherwise = "肥胖"

bmi :: IO ()
bmi = do
  putStrLn "请输入你的姓名："
  name <- getLine
  putStrLn $ "Hello! " ++ name ++ "!"
  putStrLn "请输入你的身高：(m)"
  h <- getFloat
  putStrLn "请输入你的体重：(kg)"
  w <- getFloat
  putStrLn $ (++) "BMI指数：" $ printf "%.1f" $ calBMI h w
  putStrLn $ (++) "身体状态：" $ showStatue $ calBMI h w
