module Parser where

import Data.List.NonEmpty
import Lexer

data Stmt = Let String Expr
  deriving (Show, Eq)

data Expr
  = Int Int
  | Math String Expr Expr
  | Comp
  deriving (Show, Eq)

parse :: [Token] -> [Stmt]
parse [] = []
parse (t : ts) = case nextStmt (t :| ts) of
  (stmt, []) -> [stmt]
  (stmt, tokens') -> stmt : parse tokens'

nextStmt :: NonEmpty Token -> (Stmt, [Token])
nextStmt tokens = case tokens of
  Lexer.Let :| ts -> parseLet tokens

parseLet :: NonEmpty Token -> (Stmt, [Token])
parseLet tokens = case tokens of
  Lexer.Let :| Ident i : Assign : t : ts ->
    let (expr, tokens') = parseExprToSemi (t :| ts)
     in (Parser.Let i expr, tokens')
  _ -> error "I can't figure out how to parse that. I was expecting something like `let foo = 10;`"

parseExprToSemi :: NonEmpty Token -> (Expr, [Token])
parseExprToSemi tokens = case tokens of
  Lexer.Int i :| Semicolon : ts -> (Parser.Int (read i), ts)
  _ -> error "That is not a valid expression"
