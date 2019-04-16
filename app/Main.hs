{-#LANGUAGE BinaryLiterals #-}
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
            | Character Char deriving Show

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

parseBin :: Parser LispVal
parseBin = lexeme $ do try $ string "#b"
                       stringNum <- many1 (oneOf "01")
                       return $ Number $ read("0b" ++ stringNum)

parseNumber :: Parser LispVal
parseNumber = parseNormalNum <|> parseDecimal <|> parseHex <|> parseOct <|> parseBin

parseExpr :: Parser LispVal
parseExpr = spaces >> 
    (parseList <|> parseAtom <|> parseString <|> try parseNumber
     <|> try parseBool <|> try parseCharacter)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "Failed to match " ++ show err
    Right val -> "Found value " ++ show val

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)