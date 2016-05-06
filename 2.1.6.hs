import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric

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
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter

-- Terminals --
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Data Type --
data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
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
                           _         -> val !! 0

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return . Float . fst . head . readFloat $ x ++ "." ++ y

parseNumber :: Parser LispVal
parseNumber =
    parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = liftM (Number . read) $ many1 digit

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
    try $ string "#d"
    x <- many1 octDigit
    return . Number . read $ x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return . Number . hex2dig $ x

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return . Number . oct2dig $ x

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- many1 $ oneOf "01"
    return . Number . bin2dig $ x

oct2dig = fst . head . readOct
hex2dig = fst . head . readHex
bin2dig = sum . zipWith (\b c -> if c == '1' then 2 ^ b else 0) [0..] . reverse
