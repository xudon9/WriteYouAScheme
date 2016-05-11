{-# LANGUAGE ExistentialQuantification #-}
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
     putStrLn "吼啦!"
     line <- getLine
     evaled <- return . liftM show $ readExpr line >>= eval
     putStrLn . extractValue $ trapError evaled

-- Terminals --
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

-- Data Type definition --
data LispVal = Atom         String
             | List         [LispVal]
             | DottedList   [LispVal] LispVal
             | Number       Integer
             | String       String
             | Bool         Bool    deriving(Eq)

-- Parse Basic Type --
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

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList init last) = "(" ++ unwordsList init ++ " . " ++ show last ++ ")"
showVal (Number num) = show num
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool flag) = if flag then "#t" else "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Eval --
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Atom "quote", val]) = return val
eval form@(List (Atom "cond" : xs)) = evalCond xs
    where evalCond [] = throwError $ BadSpecialForm "No true expr in cond" form
          evalCond (List (Atom "else" : vals) : _) = mapM eval vals >>= return . last
          evalCond (List (cond : vals) : rest) = do
              result <- eval cond
              case result of
                Bool False -> evalCond rest
                Bool True  -> mapM eval vals >>= return . last
                otherwise  -> throwError $ TypeMismatch "boolean" cond
eval form@(List (Atom "case" : key : xs)) = do
    evaledKey  <- eval key
    resultList <- evalCase evaledKey xs
    return $ last resultList
        where evalCase _ [] = throwError $ BadSpecialForm "No matched list in case" form
              evalCase _ (List (Atom "else" : vals) : _) = mapM eval vals
              evalCase k (List (List datums : vals) : rest) = do
                  equalities <- mapM (\x -> eqv [k, x]) datums
                  if Bool True `elem` equalities
                     then mapM eval vals
                     else evalCase k rest
              evalCase _ xs = throwError $ BadSpecialForm "Bad case" $ List xs
eval (List [Atom "if", pred, stmt1, stmt2]) = do
                     result <- eval pred
                     eval $ case result of {Bool False -> stmt2; _ -> stmt1}
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "识唔得此形式" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "不兹瓷的函数" func)
                        ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("/=", numBoolBinop (/=)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("string=?", strBoolBinop (==)),
  ("string<?", strBoolBinop (<)),
  ("string>?", strBoolBinop (>)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eq?", eqv),
  ("eqv?", eqv),
  ("equal?", equal) ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right
numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError . TypeMismatch "number" $ String n
                              else return . fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]          = return x
car [DottedList (x : _) _]  = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return . List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)]     = return . Bool $ x == y
eqv [(Number x), (Number y)] = return . Bool $ x == y
eqv [(String x), (String y)] = return . Bool $ x == y
eqv [(Atom x), (Atom y)]     = return . Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [x@(List _), y@(List _)] = eqvList eqv [x, y]
eqv [_, _]                   = return $ Bool False
eqv badArgList               = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) = do
    x' <- unpacker x
    y' <- unpacker y
    return $ x' == y'
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [x@(List _), y@(List _)] = eqvList equal [x, y]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [x, y] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals x y) [AnyUnpacker unpackNum,
                                                           AnyUnpacker unpackStr,
                                                           AnyUnpacker unpackBool]
    eqvEquals <- eqv [x, y]
    return . Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqf [(List x), (List y)] = return . Bool $ (length x == length y) && (all eqvPair $ zip x y)
    where eqvPair (a, b) = case eqf [a, b] of
                              Left err      -> False
                              Right (Bool v)-> v

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch   String LispVal
               | Parser         ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "预期" ++ show expected ++ "个参数; 实参:" ++ unwordsList found
showError (TypeMismatch expected found) = "错误类型: 期望" ++ expected ++ ", 实参:" ++ show found
showError (Parser parseErr)             = "解析错误，位置: " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "Chottomatte...出现了一点点错误哦 =_=#"
     strMsg = Default
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
