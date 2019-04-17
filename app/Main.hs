module Main where

import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

lexeme :: Parser a -> Parser a
lexeme p = p <* (skipMany space)

spaces :: Parser ()
spaces = skipMany space

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool 
            | Character Char 
            | Float Double deriving Show

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
            
instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

parseString :: Parser LispVal
parseString = lexeme $
              do char '"'
                 x <- many (escapedChars <|> (noneOf "\"\\"))
                 char '"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = lexeme $ 
            do char '#'
               x <- oneOf "tf"
               return $ case x of
                    't' -> Bool True
                    'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = lexeme $ 
           do  first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom


parseQuoted :: Parser LispVal
parseQuoted = do
            char '\''
            x <- parseExpr
            return $ List [Atom "quote", x]

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    val <- try (string "newLine" <|> string "space")
        <|> do  x <- anyChar
                notFollowedBy alphaNum
                return [x]
    return $ Character $ case val of
            "space" -> ' '
            "newLine" -> '\n'
            otherwise -> (val !! 0)

escapedChars :: Parser Char
escapedChars = do char '\\' -- escaped backslash
                  x <- oneOf "\\\"nrt" -- backslash, quote, or space chars
                  return $ case x of 
                    '\\' -> x
                    '"' -> x
                    'n' -> '\n'
                    't' -> '\t'
                    'r' -> '\r'

parseNormalNum :: Parser LispVal
parseNormalNum = lexeme $ do
                    stringNum <- many1 digit
                    return $ Number (read stringNum)

parseDecimal :: Parser LispVal
parseDecimal = lexeme $ do try $ string "#d"
                           stringNum <- many1 digit
                           return $ Number (read stringNum)

parseHex :: Parser LispVal
parseHex = lexeme $ do try $ string "#x"
                       stringNum <- many1 digit
                       return $ Number $ read ("0x" ++ stringNum)

parseOct :: Parser LispVal
parseOct = lexeme $ do try $ string "#o"
                       stringNum <- many1 digit
                       return $ Number $ read ("0o" ++ stringNum)
                       
toDec :: String -> Integer
toDec str = sum $ zipWith (*) powers (reverse nums)
  where
    nums = map (\c -> if c == '1' then 1 else 0) str
    powers = map (2^) [0..]

parseBin :: Parser LispVal
parseBin = lexeme $ do try $ string "#b"
                       stringNum <- many1 (oneOf "01")
                       return $ Number $ (toDec stringNum)

parseFloat :: Parser LispVal
parseFloat = lexeme $ do
                before <- many1 digit
                char '.'
                after <- many1 digit
                return $ Float $ read (before ++ "." ++ after)

parseList :: Parser LispVal
parseList = do
            x <- parseExpr `sepBy` spaces 
            return $ List x

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
        char '`'
        x <- parseExpr
        return $ List [Atom "quasiquote", x]            

parseUnQuote :: Parser LispVal
parseUnQuote = do
        char ','
        x <- parseExpr
        return $ List [Atom "unquote", x]

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail   

parseNumber :: Parser LispVal
parseNumber = try parseFloat <|> parseDecimal <|> try parseHex
    <|> try parseOct <|> try parseBin <|> parseNormalNum
    <|> parseQuasiQuoted <|> parseUnQuote

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber
     <|> parseBool <|> parseCharacter <|> parseQuoted
     <|> do char '('
            x <- try parseDottedList <|> try parseList
            char ')'
            return x

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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
                  ("string>=?", strBoolBinop (>=))]

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                    then throwError $ NumArgs 2 args
                    else do left <- unpacker $ args !! 0
                            right <- unpacker $ args !! 1
                            return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

symbolp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _          = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _          = Bool False

boolp :: LispVal -> LispVal
boolp   (Bool _)   = Bool True
boolp   _          = Bool False

listp :: LispVal -> LispVal
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""

string2symbol :: LispVal -> LispVal
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)
main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled