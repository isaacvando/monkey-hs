module Lexer where

import Data.Char (isDigit)
import Data.Function ((&))
import Data.List
import Prelude hiding (lex)

data Token
  = Illegal String
  | Ident String
  | Int String
  | Assign
  | Plus
  | Minus
  | Divide
  | Times
  | Lessthan
  | Greaterthan
  | Equal
  | Notequal
  | Not
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Function
  | Let
  | True_
  | False_
  | If
  | Else
  | Return
  | Eof
  deriving (Eq)

instance Show Token where
  show t = case t of
    Illegal i -> "ILLEGAL(" ++ i ++ ")"
    Ident i -> "IDENT(" ++ i ++ ")"
    Int i -> "INT(" ++ i ++ ")"
    Assign -> "="
    Plus -> "+"
    Minus -> "-"
    Divide -> "/"
    Times -> "*"
    Lessthan -> "<"
    Greaterthan -> ">"
    Equal -> "=="
    Notequal -> "!="
    Not -> "!"
    Comma -> ","
    Semicolon -> ";"
    Lparen -> "("
    Rparen -> ")"
    Lbrace -> "{"
    Rbrace -> "}"
    Function -> "FUNCTION"
    Let -> "LET"
    Eof -> "EOF"
    If -> "IF"
    Else -> "ELSE"
    True_ -> "TRUE"
    False_ -> "FALSE"
    Return -> "RETURN"

lex :: String -> [Token]
lex input = case nextToken input of
  (tok, "") -> tok : []
  (tok, input') -> tok : lex input'

nextToken :: String -> (Token, String)
nextToken input = case input of
  ' ' : xs -> nextToken xs
  '\t' : xs -> nextToken xs
  '\n' : xs -> nextToken xs
  '\r' : xs -> nextToken xs
  '!' : '=' : xs -> (Notequal, xs)
  '!' : xs -> (Not, xs)
  '=' : '=' : xs -> (Equal, xs)
  '<' : xs -> (Lessthan, xs)
  '>' : xs -> (Greaterthan, xs)
  '=' : xs -> (Assign, xs)
  '+' : xs -> (Plus, xs)
  '-' : xs -> (Minus, xs)
  '*' : xs -> (Times, xs)
  '/' : xs -> (Divide, xs)
  '(' : xs -> (Lparen, xs)
  ')' : xs -> (Rparen, xs)
  '{' : xs -> (Lbrace, xs)
  '}' : xs -> (Rbrace, xs)
  ',' : xs -> (Comma, xs)
  ';' : xs -> (Semicolon, xs)
  "" -> (Eof, "")
  c : _
    | isIdentChar c -> getIdent input
    | isDigit c -> getInteger input
  _ -> (Illegal input, "")

isIdentifier :: String -> Bool
isIdentifier input = case words input of
  word : _ -> all isIdentChar word
  _ -> False

isIdentChar :: Char -> Bool
isIdentChar c = c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

isInteger :: String -> Bool
isInteger input = case words input of
  word : _ -> all isDigit word
  _ -> False

getInteger :: String -> (Token, String)
getInteger input =
  let (num, rest) = span isDigit input
   in (Int num, rest)

getIdent :: String -> (Token, String)
getIdent input =
  let (ident, rest) = span isIdentChar input
   in case ident of
        "let" -> (Let, rest)
        "fn" -> (Function, rest)
        "true" -> (True_, rest)
        "false" -> (False_, rest)
        "if" -> (If, rest)
        "else" -> (Else, rest)
        "return" -> (Return, rest)
        _ -> (Ident ident, rest)
