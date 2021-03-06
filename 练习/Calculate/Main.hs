module Main where
import Calculate (calc, inits, Lit (Val), LitOp)
import Scanner (scanExp)
import System.Environment (getArgs)
import Control.Monad.Trans.State (evalState)

cal :: String -> LitOp
cal exp = (evalState.calc.scanExp) exp inits

num :: LitOp -> Float
num (Left (Val a)) = a
num _ = error "input error"

calculate = num.cal

main :: IO ()
main = do
    expr <- getArgs
    print $ calculate $ concat expr