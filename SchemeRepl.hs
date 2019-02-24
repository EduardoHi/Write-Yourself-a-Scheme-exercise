module SchemeRepl where

import Control.Monad.Except
import System.IO
import System.Environment
import SchemeEval
import SchemeParser(LispError,ThrowsError)
import LispVal

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = readEval env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  let loadFile = eval env (List [Atom "load", String (args !! 0)])
  (runIOThrows $ liftM show $ loadFile) >>= hPutStrLn stderr


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme >>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne $ args

