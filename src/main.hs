module Main where

import Control.Monad.Except (MonadError (..))
import Data.Array (Array, listArray)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Functor ((<&>))
import Data.Ratio (denominator, numerator, (%))
import Numeric (readFloat, readHex, readOct)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    alphaNum,
    anyChar,
    between,
    char,
    digit,
    hexDigit,
    letter,
    many,
    many1,
    noneOf,
    notFollowedBy,
    octDigit,
    oneOf,
    option,
    parse,
    sepBy,
    sepEndBy,
    skipMany,
    skipMany1,
    space,
    string,
    try,
    (<|>),
  )

main :: IO ()
main = do
  args <- getArgs
  let evaled = show <$> (readExpr (head args) >>= eval)
  putStrLn $ extractValue $ trapError evaled

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = message

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err) = error $ "unexpected error: " ++ show err

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf ['\\', '"'] <|> parseEscape
  char '"'
  return $ String x

escapes :: [(Char, Char)]
escapes =
  [ ('\'', '\''),
    ('"', '"'),
    ('\\', '\\'),
    ('a', '\a'),
    ('b', '\b'),
    ('f', '\f'),
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    ('v', '\v')
  ]

parseEscape :: Parser Char
parseEscape = do
  char '\\'
  c <- oneOf $ map fst escapes
  let (Just escaped) = lookup c escapes
  return escaped

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
  let getDouble :: Parser Double
      getDouble = do
        f <- option Nil (try parseFloat)
        case f of
          (Float f) ->
            return $ realToFrac f
          _ -> do
            (Number n) <- parseDec
            return $ fromIntegral n
   in do
        x <- getDouble
        char '+'
        y <- getDouble
        char 'i'
        return $ Complex (x :+ y)

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

eval :: LispVal -> ThrowsError LispVal
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", val]) = throwError $ Default "` is not support"
eval (List [Atom "unquoted", val]) = throwError $ Default ", is not support"
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    _ -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(List _) =
  throwError $ BadSpecialForm "Unrecognized special form" val
eval val@(DottedList _ _) = throwError $ Default ". is not support"
eval val = return val

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", unaryOp $ return . isSymbol),
    ("string?", unaryOp $ return . isString),
    ("number?", unaryOp $ return . isNumber),
    ("bool?", unaryOp $ return . isBool),
    ("list?", unaryOp $ return . isList),
    ("symbol->string", unaryOp symbol2string),
    ("string->symbol", unaryOp string2symbol),
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
    ("eqv?", eqv)
  ]

unaryOp ::
  (LispVal -> ThrowsError LispVal) ->
  [LispVal] ->
  ThrowsError LispVal
unaryOp func [param] = func param
unaryOp _ params = throwError $ NumArgs 1 params

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

symbol2string :: LispVal -> ThrowsError LispVal
symbol2string (Atom i) = return $ String i
symbol2string val = throwError $ TypeMismatch "atom" val

string2symbol :: LispVal -> ThrowsError LispVal
string2symbol (String s) = return $ Atom s
string2symbol val = throwError $ TypeMismatch "string" val

numericBinop ::
  (Integer -> Integer -> Integer) ->
  [LispVal] ->
  ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ param@[_] = throwError $ NumArgs 2 param
numericBinop op params = mapM unpackNum params <&> Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val@(String s) =
  let parsed = reads s
   in if null parsed
        then throwError $ TypeMismatch "number" val
        else return $ fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum val = throwError $ TypeMismatch "number" val

boolBinop ::
  (LispVal -> ThrowsError a) ->
  (a -> a -> Bool) ->
  [LispVal] ->
  ThrowsError LispVal
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise = do
    lhs <- unpacker $ head args
    rhs <- unpacker $ last args
    return $ Bool $ lhs `op` rhs

numBoolBinop ::
  (Integer -> Integer -> Bool) ->
  [LispVal] ->
  ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackString

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number n) = return $ show n
unpackString val = throwError $ TypeMismatch "string" val

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val = throwError $ TypeMismatch "boolean" val

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [val] = throwError $ TypeMismatch "pair" val
car val = throwError $ NumArgs 1 val

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [val] = throwError $ TypeMismatch "pair" val
cdr val = throwError $ NumArgs 1 val

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [x, y] = return $ DottedList [x] y
cons val = throwError $ NumArgs 1 val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Nil, Nil] = return $ Bool True
eqv [Atom lhs, Atom rhs] = return $ Bool (lhs == rhs)
eqv [Number lhs, Number rhs] = return $ Bool (lhs == rhs)
eqv [Float lhs, Float rhs] = return $ Bool (lhs == rhs)
eqv [Ratio lhs, Ratio rhs] = return $ Bool (lhs == rhs)
eqv [Complex lhs, Complex rhs] = return $ Bool (lhs == rhs)
eqv [String lhs, String rhs] = return $ Bool (lhs == rhs)
eqv [Char lhs, Char rhs] = return $ Bool (lhs == rhs)
eqv [Bool lhs, Bool rhs] = return $ Bool (lhs == rhs)
eqv [Vector lhs, Vector rhs] = eqv [toList lhs, toList rhs]
  where
    toList arr = List $ foldr (:) [] arr
eqv [DottedList lhs lhs', DottedList rhs rhs'] =
  eqv [toList lhs lhs', toList rhs rhs']
  where
    toList xs y = List $ xs ++ [y]
eqv [List lhs, List rhs] =
  return . Bool $ (length lhs == length rhs) && all eqv' (zip lhs rhs)
  where
    eqv' (lhs, rhs) = let (Right (Bool res)) = eqv [lhs, rhs] in res
eqv [_, _] = return $ Bool False
eqv val = throwError $ NumArgs 2 val
