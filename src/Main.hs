module Main where

import Lambda (eval, toString)
import Parser (parserlamb, lexer)
import System.IO (stdout, hFlush)
import Control.Monad (unless)

read' :: IO String
read' = do
    putStr "> "
    hFlush stdout
    getLine

eval' :: String -> IO ()
eval' = putStrLn . toString . eval . parserlamb . lexer

main :: IO ()
main = do
  input <- read'
  unless (input == ":quit") $ eval' input >> main