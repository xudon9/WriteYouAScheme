import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Ratio
import Data.Complex

main :: IO ()
main = putStrLn "善哉。" >> getLine >>= print . eval . readExpr

-- Data Type definition --
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList init last) = "(" ++ unwordsList init ++ " . " ++ show last ++ ")"
showVal (Number num) = show num
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool flag) = if flag then "#t" else "#f"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val -> val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Terminals --
symbol :: Parser Char
symbol = oneOf "#!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Data Type --
parseString :: Parser LispVal
parseString = do
    char '\"'
    x <- many $ noneOf "\""
    char '\"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> symbol <|> digit
    let atom = first : rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Recursive parser --
parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Eval --
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (String $ func ++ "? 没听说过...") ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", unaryOp (\x -> Bool $ case x of {Atom _ -> True; _ -> False})),
  ("string?", unaryOp (\x -> Bool $ case x of {String _ -> True; _ -> False})),
  ("number?", unaryOp (\x -> Bool $ case x of {Number _ -> True; _ -> False})),
  ("bool?", unaryOp (\x -> Bool $ case x of {Bool _ -> True; _ -> False})),
  ("list?", unaryOp (\x -> Bool $ case x of {List _ -> True; DottedList _ _ -> True; _ -> False}))]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
