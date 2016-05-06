import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Ratio
import Data.Complex

main :: IO ()
main = do
    putStrLn "OK..."
    expr <- getLine
    putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

-- Terminals --
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Data Type --
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList init last) = "(" ++ unwordsList init ++ " . " ++ show last ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return . Float . fst . head . readFloat $ x ++ "." ++ y

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
