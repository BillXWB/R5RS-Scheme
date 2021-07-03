{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    MonadIO (liftIO),
    MonadTrans (lift),
    runExceptT,
  )
import Data.Array (Array, listArray)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.Ratio (denominator, numerator, (%))
import Numeric (readFloat, readHex, readOct)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
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

arrayList :: Array i e -> [e]
arrayList = foldr (:) []

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [arg] -> runOne arg
    _ -> putStrLn "Program takes only 0 or 1 argument"

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl =
  primitiveBindings
    >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        varargs :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }

dotted2List :: [LispVal] -> LispVal -> LispVal
dotted2List xs y = List $ xs ++ [y]

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func params varargs body _) =
  "(lambda ("
    ++ unwords (map show params)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"

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
  | ExpectedAtom LispVal
  | Default String

showError :: LispError -> String
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " arg(s), found value(s): "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (ExpectedAtom found) = "Expected atom, found: " ++ show found
showError (Default message) = message

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

instance MonadFail ThrowsError where
  fail message = error $ "Unexpected error: " ++ message

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue val' = let (Right val) = val' in val

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom var) = getVar env var
eval _ (List [Atom "quote", val]) = return val
eval _ (List [Atom "quasiquote", val]) =
  throwError $ Default "` is not support"
eval _ (List [Atom "unquoted", val]) =
  throwError $ Default ", is not support"
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    Bool False -> eval env alt
    _ -> throwError $ TypeMismatch "boolean" result
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
    then
      throwError $
        BadSpecialForm "No true clause in cond expression: " form
    else case head clauses of
      List [Atom "else", expr] -> eval env expr
      List [test, expr] ->
        eval env $
          List [Atom "if", test, expr, List (Atom "cond" : tail clauses)]
      _ -> throwError $ BadSpecialForm "Ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
    then
      throwError $
        BadSpecialForm "No true clause in case expression: " form
    else case head clauses of
      List (Atom "else" : exprs) -> eval' env exprs
      List (List datums : exprs) -> do
        result <- eval env key
        let equality =
              any
                ( \x ->
                    let (Right (Bool res)) =
                          eqv [result, x]
                     in res
                )
                datums
        if equality
          then eval' env exprs
          else eval env $ List (Atom "case" : key : tail clauses)
      _ -> throwError $ BadSpecialForm "Ill-formed case expression: " form
  where
    eval' env exprs = mapM (eval env) exprs <&> last
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval
  env
  ( List
      (Atom "define" : DottedList (Atom var : params) varargs : body)
    ) = makeVarArgsFunc varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgsFunc varargs env params body
eval env (List (Atom "lambda" : varargs : body)) =
  makeVarArgsFunc varargs env [] body
eval env (List (func' : args')) = do
  func <- eval env func'
  args <- mapM (eval env) args'
  apply func args
eval _ val@(List _) =
  throwError $ BadSpecialForm "Unrecognized special form" val
eval _ val@(DottedList _ _) = throwError $ Default ". is not support"
eval _ val = return val

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (Func params varargs body closure) args =
  if length params /= length args && isNothing varargs
    then throwError $ NumArgs (toInteger $ length params) args
    else
      liftIO (bindVars closure $ zip params args)
        >>= bindVarArgs varargs
        >>= evalBody
  where
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
    remainingArgs = drop (length params) args
apply primitive args =
  let (PrimitiveFunc func) = primitive in liftThrows $ func args

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
    ("string-length", unaryOp stringLength),
    ("string-ref", stringRef),
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
    ("equal?", equal)
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

stringLength :: LispVal -> ThrowsError LispVal
stringLength (String s) = return . Number $ toInteger . length $ s
stringLength val = throwError $ TypeMismatch "string" val

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String s, Number i]
  | i `elem` [0 .. toInteger (length s) - 1] =
    return . Char $ s !! fromInteger i
  | otherwise = throwError $ Default "Index out of bound"
stringRef [String _, val] = throwError $ TypeMismatch "number" val
stringRef [val, _] = throwError $ TypeMismatch "string" val
stringRef val = throwError $ NumArgs 2 val

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
eqv [Vector lhs, Vector rhs] =
  eqv [List $ arrayList lhs, List $ arrayList rhs]
eqv [DottedList lhs lhs', DottedList rhs rhs'] =
  eqv [dotted2List lhs lhs', dotted2List rhs rhs']
eqv [List lhs, List rhs] = eqList eqv lhs rhs
eqv [_, _] = return $ Bool False
eqv val = throwError $ NumArgs 2 val

eqList ::
  ([LispVal] -> ThrowsError LispVal) ->
  [LispVal] ->
  [LispVal] ->
  ThrowsError LispVal
eqList eq lhs rhs =
  return . Bool $ (length lhs == length rhs) && all eq' (zip lhs rhs)
  where
    eq' (lhs, rhs) = let (Right (Bool res)) = eq [lhs, rhs] in res

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackers :: [Unpacker]
unpackers =
  [ AnyUnpacker unpackNum,
    AnyUnpacker unpackString,
    AnyUnpacker unpackBool
  ]

equal :: [LispVal] -> ThrowsError LispVal
equal [Vector lhs, Vector rhs] =
  equal [List $ arrayList lhs, List $ arrayList rhs]
equal [DottedList lhs lhs', DottedList rhs rhs'] =
  equal [dotted2List lhs lhs', dotted2List rhs rhs']
equal [List lhs, List rhs] = eqList equal lhs rhs
equal args@[lhs, rhs] = do
  primitiveEquals <- or <$> mapM (unpackEquals lhs rhs) unpackers
  (Bool eqvEquals) <- eqv args
  return $ Bool (primitiveEquals || eqvEquals)
equal val = throwError $ NumArgs 2 val

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals lhs' rhs' (AnyUnpacker unpacker) =
  do
    lhs <- unpacker lhs'
    rhs <- unpacker rhs'
    return $ lhs == rhs
    `catchError` const (return False)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (`writeIORef` value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef
    >>= extendEnv bindings
    >>= newIORef
  where
    extendEnv bindings env = (++ env) <$> mapM addBinding bindings
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

makeFunc ::
  Maybe String ->
  Env ->
  [LispVal] ->
  [LispVal] ->
  IOThrowsError LispVal
makeFunc varargs env params' body = do
  params <- mapM getParamName params'
  return $ Func params varargs body env
  where
    getParamName :: LispVal -> IOThrowsError String
    getParamName param = do
      case param of
        Atom i -> return i
        val -> throwError $ ExpectedAtom val

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgsFunc ::
  LispVal ->
  Env ->
  [LispVal] ->
  [LispVal] ->
  IOThrowsError LispVal
makeVarArgsFunc (Atom varargs) = makeFunc $ Just varargs
makeVarArgsFunc val = \_ _ _ -> throwError $ ExpectedAtom val
