module Main where

import           Bruijn        (removeNames, restoreNames)
import           Control.Monad (unless)
import           Expression    (Expression (eval))
import qualified Lambda        as Named
import           Parser        (lexer, parserlamb)
import           System.IO     (hFlush, stdout)

read' :: IO String
read' = do
  putStr "> "
  hFlush stdout
  getLine

interpret :: String -> IO ()
interpret = print . interpret' . parserlamb . lexer

interpret' :: Named.Exp -> Named.Exp
interpret' t =
  restoreNames (eval $ removeNames t gamma) gamma
  where gamma = Named.freeVariables t

main :: IO ()
main = do
  input <- read'
  unless (input == ":q") $ interpret input >> main
