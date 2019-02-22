module SchemeRepl where

import Control.Monad.Except
import System.IO
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


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = runRepl

