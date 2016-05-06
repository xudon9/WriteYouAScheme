import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = do
    (expr : _) <- getArgs
    putStrLn $ readExpr expr


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right _  -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- Terminals --
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Data Type --
data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

parseString :: Parser LispVal
parseString = do
    char '\"'
    x <- many (noneOf "\"")
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
