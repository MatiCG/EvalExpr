
import System.Environment
import System.Exit
import System.IO

main :: IO()
main = getArgs >>= parse

parse ["--usage"] = usage >> exit

parse [expression] = operate(expression) >> exit

parse args = usage >> failure "Error: Invalid Arg"

operate (expression) = do
    if (length expression >= 1)
        then do
            stack(head expression)
            operate(tail expression)
    else calculate


stack (c) = do
    if (c >= '0' && c <= '9') || c == '.'
        then print "number"
    else if c == '('
        then print "left par"
    else if c == ')'
        then print "right par"
    else if isOperator(c) == True
        then do
            print "operator"
    else do
        failure "Error: Non-allowed character found" 

calculate = do
    print("Calculate")

isOperator :: Char -> Bool
isOperator (c) = c `elem` ".+-*/"

usage = do
    putStrLn "USAGE: ./funEvalExpr expression\n"
    putStrLn "      expression        the expression to calculate"            

exit = exitWith ExitSuccess
failure :: String -> IO a
failure str = hPutStrLn stderr str >> exitWith (ExitFailure 84)