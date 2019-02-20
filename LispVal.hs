module LispVal where


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
