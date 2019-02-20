{-# LANGUAGE ExistentialQuantification #-}
module SchemeEval(readEval) where

import Control.Monad.Except
import SchemeParser

-- Evaluator

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(LispNumber _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True -> eval conseq
    r -> throwError $ TypeMismatch "bool" r

eval (List (Atom "cond":clauses)) =
  evalClauses clauses
  where evalClauses [] = throwError $ Default "no condition true in cond"
        evalClauses (c:clauses) = do 
          r <- evalCondClause c
          case r of
            Bool False -> evalClauses clauses
            otherwise -> return r
eval (List (Atom "case":key:clauses)) = do
  kValue <- eval key
  evalClauses clauses kValue
  where
    evalClauses [] _ = throwError $ Default "no condition true in cond"
    evalClauses (c:clauses) k = do
      r <- evalCaseClause c k
      case r of
        Bool False -> evalClauses clauses k
        otherwise -> return r
            
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "unrecognized special form" badForm

evalCaseClause (List [Atom "else", exp]) key = eval exp
evalCaseClause (List [List datums, exp]) key = do
  if any (==key) datums then eval exp else return $ Bool False
evalCaseClause badClause _ = throwError $ BadSpecialForm "unrecognized clause in case" badClause
               
evalCondClause (List [Atom "else", exp])= eval exp
evalCondClause (List [test, exp]) =
  do result <- eval test
     case result of
       Bool True -> eval exp
       otherwise -> return $ Bool False
evalCondClause badClause = throwError $ BadSpecialForm "unrecognized clause in cond" badClause


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = case lookup func primitives of
                    Nothing -> throwError $ NotFunction "Unrecognized primitive function args" func
                    Just f -> f args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
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
             ] ++ typeTests ++ conversions

typeTests :: [(String, [LispVal] -> ThrowsError LispVal)]
typeTests = [("boolean?", typeOp isBool),
            ("pair?", typeOp isPair),
            ("vector?", typeOp isVector),
            ("number?", typeOp isNumber),
            ("string?", typeOp isString),
            ("char?", typeOp isChar),
            ("symbol?", typeOp isSymbol)]

conversions :: [(String, [LispVal] ->ThrowsError LispVal)]
conversions = [("symbol->string", unaryOp symToString),
               ("string->symbol", unaryOp stringToSym)]


isSymbol (Atom _) = True
isSymbol _ = False

symToString (Atom a) = String $ a
stringToSym (String a) = Atom $ a

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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [] = throwError $ NumArgs 1 []
unaryOp op params = if length params > 1
                    then throwError $ NumArgs (toInteger . length $ params) params
                    else return . op $ head params
                    
typeOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
typeOp op [] = throwError $ NumArgs 1 []
typeOp op params = return $ Bool $ if length params > 1
                                   then False
                                   else op $ head params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
              
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op            [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- numericBinop op params = LispNumber $ LispInteger $ foldl1 op $ map unpackVal params
numericBinop op        params = mapM unpackNum params >>= return . (LispNumber . LispInteger) . foldl1 op

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LispNumber (LispInteger n)) = return n
unpackNum notNum                       = throwError $ TypeMismatch "number" notNum

-- list primitives

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- equivalences

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(LispNumber arg1), (LispNumber arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                (return $ unpacked1 == unpacked2) `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


readEval :: String -> IO String
readEval s = return $ extractValue $ trapError $ liftM show $ readExpr s >>= eval
  
-- main :: String -> IO ()
-- main s = do
--   -- args <- getArgs
--   evaled <- return $ readEval s
--   putStrLn $ extractValue $ trapError evaled

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val