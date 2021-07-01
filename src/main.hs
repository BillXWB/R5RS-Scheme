module Main where

import Data.Array
import Data.Complex
import Data.Ratio
import Numeric (readFloat, readHex, readOct)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn $ readExpr expr

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

data LispVal
  = Nil
  | Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | String String
  | Char Char
  | Bool Bool

showVal :: LispVal -> String
showVal Nil = "nil"
showVal (Atom a) = a
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Ratio r) = show (numerator r) ++ "/" ++ show (denominator r)
showVal (Complex c) = show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
showVal (String s) = "\"" ++ s ++ "\""
showVal (Char c) =
  "#\\" ++ case c of
    ' ' -> "space"
    '\n' -> "newline"
    _ -> [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList heads tail) =
  "(" ++ unwordsList heads ++ " . " ++ showVal tail ++ ")"
showVal (Vector v) = "#(" ++ unwordsList (foldr (:) [] v) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf ['\\', '"'] <|> parseEscape
  char '"'
  return $ String x

parseEscape :: Parser Char
parseEscape = do
  char '\\'
  c <- anyChar
  return $ case c of
    '\'' -> '\''
    '"' -> '"'
    '\\' -> '\\'
    'a' -> '\a'
    'b' -> '\b'
    'f' -> '\f'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'v' -> '\v'
    _ -> error $ "invalid escape: " ++ [c]

parseChar :: Parser LispVal
parseChar =
  let alphabetic = do x <- anyChar; notFollowedBy alphaNum; return x
   in do
        string "#\\"
        x <-
          try $
            (string "space" >> return ' ')
              <|> (string "newline" >> return '\n')
              <|> alphabetic
        return $ Char x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  return $ Atom (first : rest)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseBin <|> parseOct <|> parseDec <|> parseHex

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 $ oneOf "01"
  let d = readBin x where readBin x = readBin' 0 x
      readBin' v [] = v
      readBin' v (x : xs) =
        let v' = v * 2 + (if x == '1' then 1 else 0)
         in readBin' v' xs
  return $ Number d

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  let [(d, _)] = readOct x
  return $ Number d

parseDec :: Parser LispVal
parseDec = parseDec' <|> parseDec''
  where
    parseDec' = Number . read <$> many1 digit
    parseDec'' = do
      try $ string "#d"
      parseDec'

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  let [(d, _)] = readHex x
  return $ Number d

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  let [(d, _)] = readFloat (x ++ "." ++ y)
  return $ Float d

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  let d = read x % read y
  return $ Ratio d

parseComplex :: Parser LispVal
parseComplex =
  let toDouble :: LispVal -> Double
      toDouble (Float x) = realToFrac x
      toDouble (Number x) = fromIntegral x
      toDouble x = error $ "type error: " ++ show x
   in do
        x <- try parseFloat <|> parseDec
        char '+'
        y <- try parseFloat <|> parseDec
        char 'i'
        return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = between beg end parseList'
  where
    beg = char '(' >> skipMany space
    end = skipMany space >> char ')'
    parseList' = do
      heads <- sepEndBy parseExpr spaces
      tail <- option Nil (char '.' >> spaces >> parseExpr)
      return $ case tail of
        Nil -> List heads
        _ -> DottedList heads tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquoted", x]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  arr <- sepBy parseExpr spaces
  char ')'
  let vec = listArray (0, length arr - 1) arr
  return $ Vector vec

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar
    <|> parseQuoted
    <|> parseQuasiquote
    <|> parseUnquoted
    <|> parseVector
    <|> parseList
