module Main where


data Token = Illegal 
    | Eof
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

instance Show Token where
    show t = case t of
        Illegal -> "ILLEGAL"
        Eof -> "EOF"
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


main :: IO ()
main = putStrLn "Hello, Haskell!"
