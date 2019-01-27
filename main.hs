
module Main (main) where

import System.Environment
import AxoParser

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn expr
         putStrLn (readExpr expr)
