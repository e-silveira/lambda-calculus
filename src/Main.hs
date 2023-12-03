module Main where

import Bruijn (removeNames, restoreNames)
import Control.Monad (unless)
import Control.Monad.Writer (runWriter)
import Expression (Expression (eval, evalAccum))
import qualified Lambda as Named
import Parser (lexer, parserlamb)
import System.IO (hFlush, stdout)

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

interpret :: String -> IO ()
interpret = interpret' . parserlamb . lexer

interpret' :: Named.Exp -> IO ()
interpret' t = do
  let (_, history) = runWriter $ evalAccum $ removeNames t gamma
  putStr $ unlines $ map (show . (flip restoreNames gamma)) history
  where
    gamma = Named.freeVariables t

main :: IO ()
main = do
  input <- read'
  unless (input == ":q") $ interpret input >> main
