module EvalExpr where

import System.Environment
import System.Exit
import System.IO
import Text.Read

evalExpr :: IO()
evalExpr = getArgs >>= parse

data Rpn = Rpn {output :: String, stack :: String}

data Precedences = Precedences {pow :: Int, mult :: Int, division :: Int, plus :: Int, minus :: Int}
precedence = Precedences 4 3 3 2 2

parse ["--usage"] = usage >> exit

parse [expression] = do
    let rpn = Rpn "" ""

    if getNumber(expression) == Nothing
        then evaluate expression rpn '0'
    else putStrLn expression
    exit

parse args = usage >> failure "Error: Invalid Arg"

evaluate:: String -> Rpn -> Char -> IO()
evaluate expression rpn lastc = do

    if (length expression >= 1)
        then do
            let operated = operate (head expression) lastc rpn
            if output operated == "Error"
                then failure "Error: Non-allowed character found"
            else evaluate(tail expression) operated (head expression)
    else do 
        let finalRpn = checkStack rpn
        calculate finalRpn

checkStack :: Rpn -> Rpn
checkStack rpn = do
    if length (stack rpn) >= 1
        then checkStack (Rpn (output rpn ++ " " ++ charToString(head (stack rpn))) (tail (stack rpn)))
    else rpn

operate :: Char -> Char -> Rpn -> Rpn
operate c lastc rpn = do
    if (c >= '0' && c <= '9') || c == '.'
        then do
            if (lastc >= '0' && lastc <= '9') || lastc == '.'
                then Rpn (output rpn ++ charToString c) (stack rpn)
            else Rpn (output rpn ++ " " ++ charToString c) (stack rpn)
    else if c == '('
        then Rpn (output rpn) (charToString c ++ stack rpn)
    else if c == ')'
        then rightParenthesisAction rpn
    else if isOperator(c) == True
        then operatorAction c rpn
    else Rpn "Error" "Error"

operatorAction :: Char -> Rpn -> Rpn
operatorAction o rpn = do
    if length (stack rpn) >= 1 && head (stack rpn) /= '('
        then do
            if isPrioritary o (head (stack rpn)) == False
                then operatorAction o (Rpn (output rpn ++ " " ++ charToString(head (stack rpn))) (tail (stack rpn)))
            else Rpn (output rpn) (charToString o ++ stack rpn)
    else Rpn (output rpn) (charToString o ++ stack rpn)

isPrioritary :: Char -> Char -> Bool
isPrioritary a b = do
    let ap = getOperatorPrecedence a
    let bp = getOperatorPrecedence b

    if ap == bp && a == '^'
        then True
    else do
        if ap > bp
            then True
        else False

getOperatorPrecedence :: Char -> Int
getOperatorPrecedence o = do
    if o == '^'
        then pow precedence
    else if o == '*'
        then mult precedence
    else if o == '/'
        then division precedence
    else if o == '-'
        then minus precedence
    else plus precedence

rightParenthesisAction :: Rpn -> Rpn
rightParenthesisAction rpn = do
    if length (stack rpn) >= 1 && head (stack rpn) /= '('
        then do
            let new = Rpn (output rpn ++ " " ++ charToString (head (stack rpn))) (tail (stack rpn))
            rightParenthesisAction new
    else Rpn (output rpn) (tail (stack rpn))

calculate rpn = do
    print(output rpn)
    print(stack rpn)

charToString :: Char -> String
charToString c = [c]

getNumber :: String -> Maybe Double
getNumber str = readMaybe str :: Maybe Double

isOperator :: Char -> Bool
isOperator c = c `elem` ".+-*/^()"

usage = do
    putStrLn "USAGE: ./funEvalExpr expression\n"
    putStrLn "      expression        the expression to calculate"            

exit = exitWith ExitSuccess

failure :: String -> IO a
failure str = hPutStrLn stderr str >> exitWith (ExitFailure 84)