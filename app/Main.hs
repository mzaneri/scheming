module Main where

import Numeric 
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

lexeme :: Parser a -> Parser a
lexeme p = p <* (skipMany space)

spaces :: Parser ()
spaces = skipMany space

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool deriving Show

parseList :: Parser LispVal
parseList = do  lexeme $ char '('
                x <- parseExpr `sepBy` spaces
                lexeme $ char ')'
                return $ List x

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

escapedChars :: Parser Char
escapedChars = do char '\\' -- escaped backslash
                  x <- oneOf "\\\"nrt" -- backslash, quote, or space chars
                  return $ case x of 
                    '\\' -> x
                    '"' -> x
                    'n' -> '\n'
                    't' -> '\t'
                    'r' -> '\r'

oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ readOct x !! 0

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ readHex x !! 0

bin2dig :: [Char] -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num t => t -> [Char] -> t
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + ( if x == '0' then 0 else 1) in 
            bin2dig' old xs

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
                       return $ Number (hex2dig stringNum)

parseOct :: Parser LispVal
parseOct = lexeme $ do try $ string "#o"
                       stringNum <- many1 digit
                       return $ Number (oct2dig stringNum)

parseBin :: Parser LispVal
parseBin = lexeme $ do try $ string "#b"
                       stringNum <- many1 (oneOf "01")
                       return $ Number (bin2dig stringNum)

parseNumber :: Parser LispVal
parseNumber = parseNormalNum <|> parseDecimal <|> parseHex <|> parseOct <|> parseBin

parseExpr :: Parser LispVal
parseExpr = spaces >> 
    (parseList <|> parseAtom <|> parseString <|> parseNumber <|> parseBool)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Failed to match " ++ show err
    Right val -> "Found value " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)