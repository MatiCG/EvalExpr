module Operations where

import Text.Read
import Numeric
import System.Exit
import System.IO

data Result = Result {result :: Double, numbers :: [String]}

calculate :: String -> IO()
calculate finalRpn = do
    if head finalRpn == ' '
        then failure "Error"
    else do
        let list = strToWordArray ' ' finalRpn
        processRpn list (Result 0 [])

processRpn :: [String] -> Result -> IO()
processRpn finalRpn res = do
    if (length finalRpn >= 1)
        then do
            let newR = processString (head finalRpn) res
            if head (numbers newR) == "Error"
                then failure "Error while calculating"
            else
                processRpn (tail finalRpn) newR
    else do
        let num = read (head (numbers res)) :: Float
        putStrLn (showFFloat (Just 2) num "")

processString :: String -> Result -> Result
processString str res = do
    let nb = getNumber str
    if nb == Nothing && length str == 1 && isOperator (head str) == True
        then do
            if length (numbers res) < 2
                then Result 0 ["Error"]
            else do
                let o2 = head (numbers res)
                let o1 = (numbers res) !! 1
                let r = evaluateToken (head str) o1 o2
                if r == "Error"
                    then Result 0 ["Error"]
                else do
                    let newStack = r:(tail (tail (numbers res)))
                    Result (read r :: Double) newStack
    else Result (result res) (str:(numbers res))

evaluateToken :: Char -> String -> String -> String
evaluateToken token o1 o2 = do
    let n1 = read o1 :: Double
    let n2 = read o2 :: Double

    if token == '^'
        then show (n1 ** n2)
    else if token == '*'
        then show (n1 * n2)
    else if token == '/'
        then do
            if n2 == 0
                then "Error"
            else show (n1 / n2)
    else if token == '-'
        then show (n1 - n2)
    else show (n1 + n2)

strToWordArray :: Char -> String -> [String]
strToWordArray delim str = foldr f [""] str
  where f :: Char -> [String] -> [String]
        f current allStrings@(partialString:handledStrings)
          | current == delim = "":allStrings
          | otherwise = (current:partialString):handledStrings

isOperator :: Char -> Bool
isOperator c = c `elem` ".+-*/^()"

getNumber :: String -> Maybe Double
getNumber str = readMaybe str :: Maybe Double

failure :: String -> IO a
failure str = hPutStrLn stderr str >> exitWith (ExitFailure 84)