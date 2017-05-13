module Main where

import           Control.Monad
import           Control.Monad.Error
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispVal where
  show = showVal

instance Show LispError where
  show = showError

instance Error LispError where
  noMsg = Default "An error occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- Catching a Left case is explicitly a programmer error by impliying it here

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (letter <|> oneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

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
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t)  = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "unrecognized primitive function" func) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) ->[LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed then throwError $ TypeMismatch "number" $ String n
                           else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
