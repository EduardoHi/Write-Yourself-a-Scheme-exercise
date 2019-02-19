module AxoParser
  ( LispVal(..)
  , LispNumber (..)
  , parseReal
  , readExpr
  ) where

import System.Environment
import Control.Monad
import Data.Void
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

data LispNumber = LispComplex Double Double
                | LispReal Double
                | LispRational Integer Integer
                | LispInteger Integer deriving(Eq)

data LispVal = Atom String
             | List [LispVal]
             | Vector [LispVal]
             | DottedList [LispVal] LispVal
             | LispNumber LispNumber
             | String String
             | Character Char
             | Bool Bool deriving (Eq)


instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom a) = a
showVal (LispNumber n) = showNum n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Character c) = "#\\" ++ (case c of
                                    ' ' -> "space"
                                    '\n' -> "newline"
                                    _ -> [c])                        
showVal (Bool b) = if b then "#t" else "#f"
showVal (List l) = "(" ++ (unwordsList l) ++ ")"
showVal (Vector v) = "#(" ++ (unwordsList v) ++ ")"
showVal (DottedList h t) = "(" ++ (unwordsList h) ++ " . " ++ (showVal t) ++ ")"

showNum :: LispNumber -> String
showNum (LispComplex r i) = (show r) ++ (if i < 0 then (show i) else "+" ++ (show i)) ++ "i"
showNum (LispReal r) = show r
showNum (LispRational n d) = (show n) ++ "/" ++ (show d)
showNum (LispInteger i) = show i
  
unwordsList :: [LispVal] -> String            
unwordsList = unwords . (map showVal)


-- Grammar
--
-- Expr = Atom | StringLiteral | Number | Quoted | QuasiQuoted | List | DottedList
--
-- Atom = (letter | symbol)? (letter | digit | symbol)*
--
-- StringLiteral = " charLiteral "
--
-- Number = Complex | Real | Rational | Integer
-- Integer = WithBase | (digit)+
-- WithBase = # (b | o | x | d) IntegerInRadix (the ommited "integer in radix" rule, checks that the digits correspond to the current radix being parsed)
-- Rational = Integer / Integer
-- Real = [+ | -] Float
-- Float = Integer . Integer
-- Complex = [Real | Rational | Integer] (+ | -) [Real | Rational | Integer] 'i'
--
-- Quoted = ' Expr
--
-- QuasiQuoted = ` QuasiList | QuasiExpr
-- QuasiList = '(' (QuasiExpr spaces)+ ')'
-- QuasiExpr = [,] Expr
--
-- List = '(' ExprSequence [DotExpr] ')'
-- ExprSequence = (Expr spaces)+
-- DotExpr = . Expr

-- this two rules are actually not used, they are only informative,
-- the other List rule is the one that handles both cases
-- List = '(' ExprSequence ')'
-- DottedList = '(' ExprSequence DotExpr ')'

type Parser = Parsec Void String

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipSome space1

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> do n <- parseNumber
                return (LispNumber n)
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseList
         <?> "Expression"


parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  val <- (string "space" <|> string "newline" <|> some alphaNumChar)
  return $ case val of
    "space" -> Character ' '
    "newline" -> Character '\n'
    _ -> Character $ head val

parseString :: Parser LispVal
parseString = do
  val <- char '"' >> manyTill L.charLiteral (char '"')
  return (String val)

-- BEHOLD, THE NUMERIC TOWER !

parseReal :: Parser LispNumber
parseReal = do
  si <- optional $ oneOf ['+','-']
  v <- L.float
  return $ case si of
    Just '-' -> LispReal (-v)
    _ -> LispReal v

parseRational = do
  LispInteger n <- parseInteger
  char '/'
  LispInteger d <- parseInteger
  return (LispRational n d)

parseImaginary = do
  si <- oneOf ['+','-']
  img <- optional (try parseReal <|> try parseRational <|> parseInteger)
  char 'i'
  let imaginary = case img of
        Just (LispReal r) -> r
        Just (LispRational n d) -> fromRational $ (toRational (fromIntegral n / fromIntegral d))
        Just (LispInteger i) -> fromInteger i
        Nothing -> 1
        _ -> 0
  return $ (if si == '+' then 1 else -1) * imaginary

parseComplex :: Parser LispNumber
parseComplex = do
  maybeReal <- optional (try parseReal <|> try parseRational <|> parseInteger)
  let real = case maybeReal of
        Just (LispReal r) -> r
        Just (LispRational n d) -> fromRational $ (toRational (fromIntegral n / fromIntegral d))
        Just (LispInteger i) -> fromInteger i
        _ -> 0
  img <- parseImaginary
  return $ LispComplex real img 

parseInteger :: Parser LispNumber
parseInteger = parseWithBase
               <|> (do
                       val <- some digitChar
                       return $ (LispInteger . read) val)

parseNumber :: Parser LispNumber
parseNumber = try parseComplex
              <|> try parseReal
              <|> try parseRational
              <|> parseInteger

parseWithBase :: Parser LispNumber
parseWithBase = do
  char '#'
  ch <- oneOf ['b','o','x','d']
  p <- case ch of
    'b' -> L.binary
    'o' -> L.octal
    'x' -> L.hexadecimal
    'd' -> L.decimal
  return (LispInteger p)

-- the end of the Numeric Tower haha

-- Lists

parseList :: Parser LispVal
parseList = do
  char '('
  head <- parseExprSequence
  tail <- optional parseDotExpr
  char ')'
  return $ case tail of
             Nothing -> List head
             Just exp -> DottedList head exp

parseExprSequence :: Parser [LispVal]
parseExprSequence = sepEndBy1 parseExpr space

parseDotExpr :: Parser LispVal
parseDotExpr = char '.' >> space >> parseExpr

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- Quasiquotation

parseQuasiList :: Parser LispVal
parseQuasiList = do
  char '('
  l <- sepBy1 parseQuasiExpr spaces
  char ')'
  return $ List l

parseQuasiExpr :: Parser LispVal
parseQuasiExpr = do
  c <- optional $ char ','
  e <- parseExpr
  return $ case c of
             Just _ -> List [Atom "unquote", e]
             _ -> e

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseQuasiList <|> parseQuasiExpr
  return $ List [Atom "quasiquote", x]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  x <- parseList
  char ')'
  return $ Vector [x]

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letterChar <|> symbol
              rest <- many (letterChar <|> digitChar <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom                       


-- Evaluator


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(LispNumber _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)] ++ typeTests ++ conversions

typeTests :: [(String, [LispVal] -> LispVal)]
typeTests = [("boolean?", typeOp isBool),
            ("pair?", typeOp isPair),
            ("vector?", typeOp isVector),
            ("number?", typeOp isNumber),
            ("string?", typeOp isString),
            ("char?", typeOp isChar),
            ("symbol?", typeOp isSymbol)]

conversions :: [(String, [LispVal] -> LispVal)]
conversions = [("symbol->string", unaryOp symToString)]

isSymbol (Atom _) = True
isSymbol _ = False

symToString (Atom a) = String $ show a

isBool (Bool _) = True
isBool _ = False

isPair (List _) = True
isPair _ = False

isVector (Vector _) = True
isVector _ = False 

isNumber (LispNumber _) = True
isNumber _ = False

isString (String _) = True
isString _ = False

isChar (Character _) = True
isChar _ = False

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op params = if length params > 1
                    then Bool False
                    else op $ head params
                    
typeOp :: (LispVal -> Bool) -> [LispVal] -> LispVal
typeOp op params = Bool $ if length params > 1
               then False
               else op $ head params               
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = LispNumber $ LispInteger $ foldl1 op $ map unpackVal params

unpackVal :: LispVal -> Integer
unpackVal (LispNumber (LispInteger n)) = n
unpackVal _ = 0 --error

unpackNum :: LispNumber -> Integer
-- unpackNum (LispComplex n _) = n
-- unpackNum (LispReal n) = n
-- unpackNum (LispRational n d) = (fromIntegral n) / (fromIntegral d)
unpackNum (LispInteger n) = n
unpackNum _ = 0 --error

readEval :: String -> LispVal
readEval = eval . readExpr
