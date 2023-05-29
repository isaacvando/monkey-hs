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
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Function
  | Let
  | Eof
  deriving (Eq)

instance Show Token where
  show t = case t of
    Illegal i -> "ILLEGAL(" ++ i ++ ")"
    Ident i -> "IDENT(" ++ i ++ ")"
    Int i -> "INT(" ++ i ++ ")"
    Assign -> "="
    Plus -> "+"
    Comma -> ","
    Semicolon -> ";"
    Lparen -> "("
    Rparen -> ")"
    Lbrace -> "{"
    Rbrace -> "}"
    Function -> "FUNCTION"
    Let -> "LET"
    Eof -> "EOF"

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
  '=' : xs -> (Assign, xs)
  '+' : xs -> (Plus, xs)
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
        _ -> (Ident ident, rest)
