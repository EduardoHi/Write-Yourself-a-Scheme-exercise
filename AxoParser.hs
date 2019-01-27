module AxoParser
  ( LispVal(..)
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
                | LispInteger Integer deriving(Eq, Show)

data LispVal = Atom String
             | List [LispVal]
             | Vector [LispVal]
             | DottedList [LispVal] LispVal
             | LispNumber LispNumber
             | String String
<<<<<<< HEAD
             | Bool Bool deriving (Eq, Show)


showVal :: LispVal -> String
showVal (Atom a) = a
-- showVal (LispNumber n) =
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool b) = if b then "#t" else "#f"
showVal (List l) = "(" ++ (unwordsList l) ++ ")"
showVal (Vector v) = "#(" ++ (unwordsList v) ++ ")"
showVal (DottedList h t) = "(" ++ (unwordsList h) ++ " . " ++ (showVal t) ++ ")"
  
unwordsList :: [LispVal] -> String            
unwordsList = unwords . (map showVal)

=======
             | Character Char
             | Bool Bool deriving (Eq, Show)


>>>>>>> d63a759... Initial commit
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
<<<<<<< HEAD
-- WithBase = # (b | o | x | d) IntegerInRadix (the ommited "integer in radix" rule, checks that the digits correspond to the current radix being parsed)
=======
-- WithBase = # (b | o | x | d) IntegerInRadix (this is a bit more logicfully)
>>>>>>> d63a759... Initial commit
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
<<<<<<< HEAD
-- List = '(' ExprSequence [DotExpr] ')'
-- ExprSequence = (Expr spaces)+
-- DotExpr = . Expr

-- this two rules are actually not used, they are only informative,
-- the other List rule is the one that handles both cases
-- List = '(' ExprSequence ')'
=======
-- List = '(' ExprSequence ')'
-- ExprSequence = (Expr spaces)+
-- (Exprs spaces)+
-- DotExpr = [. Expr]
>>>>>>> d63a759... Initial commit
-- DottedList = '(' ExprSequence DotExpr ')'

type Parser = Parsec Void String

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipSome space1

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> do n <- parseNumber
                return (LispNumber n)
         <|> parseQuoted
         <|> parseQuasiQuoted
<<<<<<< HEAD
         <|> parseList
         -- <|> do char '('
         --        x <- try parseList <|> parseDottedList
         --        char ')'
         --        return x
         <?> "Expression"
=======
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  val <- (string "space" <|> string "newline" <|> some alphaNumChar)
  return $ case val of
    "space" -> Character ' '
    "newline" -> Character '\n'
    _ -> Character $ head val
>>>>>>> d63a759... Initial commit

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

<<<<<<< HEAD
=======

>>>>>>> d63a759... Initial commit
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
<<<<<<< HEAD
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
=======
parseList = liftM List $ sepBy1 parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy1 parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
>>>>>>> d63a759... Initial commit

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
