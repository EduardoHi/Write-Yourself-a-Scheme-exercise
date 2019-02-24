module SchemeParser
  ( LispError(..)
  , ThrowsError
  , readExpr
  , readExprList
  , unwordsList
  ) where

import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.Void
import Numeric
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

import LispVal

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ ParserErr err
    Right val -> return val

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr


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
parseExprSequence = sepEndBy parseExpr space

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
