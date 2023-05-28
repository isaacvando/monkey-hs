module Lexer where
import Prelude hiding (lex)
import Data.Function ((&))

data Token = Illegal 
    | Ident String
    | Int
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
    deriving Eq

instance Show Token where
    show t = case t of
        Illegal -> "ILLEGAL"
        Ident i -> "IDENT(" ++ i ++ ")"
        Int -> "INT"
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


lex :: String -> [Token]
lex input = case nextToken input of
    (tok, "") -> tok : []
    (tok, input') -> tok : lex input'


nextToken :: String -> (Token, String)
nextToken input = case input of
    '=':xs -> (Assign, xs)
    '+':xs -> (Plus, xs)
    '(':xs -> (Lparen, xs)
    ')':xs -> (Rparen, xs)
    '{':xs -> (Lbrace, xs)
    '}':xs -> (Rbrace, xs)
    ',':xs -> (Comma, xs)
    ';':xs -> (Semicolon, xs)
    _ -> (Illegal, "")
