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
            | Bool Bool 
            | Character Char 
            | Float Double deriving Show

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

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Failed to match " ++ show err
    Right val -> "Found value " ++ show val


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)