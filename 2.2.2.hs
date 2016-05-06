import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

main :: IO ()
main = do
    putStrLn ">>>> "
    expr <- getLine
    putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right v -> "Found value " ++ show v

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)
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
             | Vector (Array Int LispVal)
             | DottedList [LispVal] LispVal
             | Complex (Complex Double)
             | Number Integer
             | Float Double
             | String String
             | Bool Bool
             | Ratio Rational
             | Character Char deriving (Show)

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\\\"tnr"
    return $ case x of
               't' -> '\t'
               'n' -> '\n'
               'r' -> '\r'
               _   -> x

parseString :: Parser LispVal
parseString = do
    char '\"'
    -- 注意一定要优先尝试转义字符 --
    x <- many $ escapedChars <|> noneOf "\""
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

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    val <- try (string "newline" <|> string "space") <|>
        do {x <- anyChar; notFollowedBy alphaNum; return [x]}
    return . Character $ case val of
                           "space"   -> ' '
                           "newline" -> '\n'
                           _         -> head val

parseComplex :: Parser LispVal
parseComplex = do
    x <- try parseFloat <|> parseDec1 <|> parseDec2
    c <- oneOf "+-"
    y <- try parseFloat <|> parseDec1 <|> parseDec2
    char 'i'
    return . Complex $ toDouble x :+ toDouble y * if c == '+' then 1 else -1

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return . Float . fst . head . readFloat $ x ++ "." ++ y

parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return . Ratio $ read x % read y

parseNumber :: Parser LispVal
parseNumber =
    parseDec1 <|> parseDec2 <|> parseHex <|> parseOct <|> parseBin

parseDec1 :: Parser LispVal
parseDec1 = liftM (Number . read) $ many1 digit

parseDec2 :: Parser LispVal
parseDec2 = try (string "#d") >> many1 octDigit >>= return . Number . read

parseHex :: Parser LispVal
parseHex = try (string "#x") >> many1 hexDigit >>= return . Number . hex2dig
  where hex2dig = fst . head . readHex

parseOct :: Parser LispVal
parseOct = try (string "#o") >> many1 octDigit >>= return . Number . oct2dig
  where oct2dig = fst . head . readOct

parseBin :: Parser LispVal
parseBin = try (string "#b") >> many1 (oneOf "01") >>= return . Number . bin2dig
  where bin2dig = sum . zipWith (\b c -> if c == '1' then 2 ^ b else 0) [0..] . reverse

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

-- Recursive parser --
parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List

parseVector :: Parser LispVal
parseVector = do
    arrayVals <- sepBy parseExpr spaces
    return . Vector $ listArray (0, length arrayVals - 1) arrayVals

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= \x -> return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    x <- char '`' >> parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    x <- char ',' >> parseExpr
    return $ List [Atom "unquote", x]
