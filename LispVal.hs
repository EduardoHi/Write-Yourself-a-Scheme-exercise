module LispVal where

import Data.Void
import Text.Megaparsec (ParseErrorBundle)
import Data.IORef
import Control.Monad.Except
import System.IO

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
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String],
                      vararg  :: (Maybe String),
                      body    :: [LispVal],
                      closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom a)           = a
showVal (LispNumber n)     = showNum n
showVal (String s)         = "\"" ++ s ++ "\""
showVal (Character c)      = "#\\" ++ (case c of
                                    ' ' -> "space"
                                    '\n' -> "newline"
                                    _ -> [c])                        
showVal (Bool b)           = if b then "#t" else "#f"
showVal (List l)           = "(" ++ (unwordsList l) ++ ")"
showVal (Vector v)         = "#(" ++ (unwordsList v) ++ ")"
showVal (DottedList h t)   = "(" ++ (unwordsList h) ++ " . " ++ (showVal t) ++ ")"

showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func params vararg body closure) =
   "(lambda (" ++ unwords (map show params) ++
      (case vararg of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


showNum :: LispNumber -> String
showNum (LispComplex r i)  = (show r) ++ (if i < 0 then (show i) else "+" ++ (show i)) ++ "i"
showNum (LispReal r)       = show r
showNum (LispRational n d) = (show n) ++ "/" ++ (show d)
showNum (LispInteger i)    = show i
  
unwordsList :: [LispVal] -> String            
unwordsList                = unwords . (map showVal)



type ParseError = ParseErrorBundle String Void

data LispError = NumArgs Integer [LispVal]
                 | TypeMismatch String LispVal
                 | ParserErr ParseError
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (ParserErr parseErr)          = "Parse error at " ++ show parseErr

instance Show LispError where show = showError




type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO
