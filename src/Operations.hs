module Operations where

import Text.Read

data Result = Result {result :: Double, numbers :: [String]}

calculate :: String -> IO()
calculate finalRpn = do
    let list = strToWordArray ' ' finalRpn
    processRpn list (Result 0 [])

processRpn :: [String] -> Result -> IO()
processRpn finalRpn res = do
    if (length finalRpn >= 1)
        then do
            let newR = processString (head finalRpn) res
            processRpn (tail finalRpn) newR
    else do
        printResult (result res)

printResult :: (Show a, RealFrac a) => a -> IO ()
printResult nb
  | a == 0 = print b
  | otherwise = print nb
  where (b, a) = properFraction nb

processString :: String -> Result -> Result
processString str res = do
    let nb = getNumber str
    if nb == Nothing && length str == 1 && isOperator (head str) == True
        then do
            let o2 = head (numbers res)
            let o1 = (numbers res) !! 1
            let r = evaluateToken (head str) o1 o2
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
        then show (n1 / n2)
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

