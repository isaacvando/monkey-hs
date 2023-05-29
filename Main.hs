module Main where

import Lexer

main :: IO ()
main = repl

repl :: IO ()
repl = do
  line <- getLine
  print $ Lexer.lex line
  repl
