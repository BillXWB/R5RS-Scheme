module Main where

import Numeric (readHex, readOct)
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
  Right val ->
    "Found value: "
      ++ case val of
        String s -> "String " ++ s
        v -> show v

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving (Show)

parseString :: Parser LispVal
parseString = do
  try $ char '"'
  x <- many $ noneOf ['\\', '"'] <|> parseEscape
  char '"'
  return $ String x

parseEscape :: Parser Char
parseEscape = do
  try $ char '\\'
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

parseAtom :: Parser LispVal
parseAtom = do
  first <- try $ letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  return $ Atom (first : rest)

parseBool :: Parser LispVal
parseBool = do
  try $ char '#'
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

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool
