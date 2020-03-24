
import System.Environment
import System.Exit

main :: IO()
main = getArgs >>= parse

parse [] = usage >> exit

parse [expression] = operate(expression) >> exit

parse args = usage >> failure

operate (expression) = do
    putStrLn "OK"

isAllowedChar :: Char -> Bool
isAllowedChar c = do
    if (c < '0' || c > '9') && isOperator(c) == False
        then True
        else False

isOperator :: Char -> Bool
isOperator c = c `elem` ".+-*/()"

usage = do
    putStrLn "USAGE: ./funEvalExpr expression\n"
    putStrLn "      expression        the expression to calculate"            

exit = exitWith ExitSuccess
failure = exitWith (ExitFailure 84)