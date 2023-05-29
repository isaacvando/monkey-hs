import Lexer
import Test.Hspec
import Text.RawString.QQ (r)
import Prelude hiding (lex)

main :: IO ()
main = hspec $ do
  describe "lexer" $ do
    it "lexes symbols" $ do
      lex "=+(){},;"
        `shouldBe` [ Assign,
                     Plus,
                     Lparen,
                     Rparen,
                     Lbrace,
                     Rbrace,
                     Comma,
                     Semicolon
                   ]

    it "lexes a full code block" $ do
      let input =
            [r|
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
            x + y;
            };
            let result = add(five, ten);
            |]
       in lex input
            `shouldBe` [ Let,
                         Ident "five",
                         Assign,
                         Int "5",
                         Semicolon,
                         Let,
                         Ident "ten",
                         Assign,
                         Int "10",
                         Semicolon,
                         Let,
                         Ident "add",
                         Assign,
                         Function,
                         Lparen,
                         Ident "x",
                         Comma,
                         Ident "y",
                         Rparen,
                         Lbrace,
                         Ident "x",
                         Plus,
                         Ident "y",
                         Semicolon,
                         Rbrace,
                         Semicolon,
                         Let,
                         Ident "result",
                         Assign,
                         Ident "add",
                         Lparen,
                         Ident "five",
                         Comma,
                         Ident "ten",
                         Rparen,
                         Semicolon,
                         Eof
                       ]

    it "lexes more symbols" $ do
      let input =
            [r|
            !-/*5;
            5 < 10 > 5;
            |]
       in lex input
            `shouldBe` [ Not,
                         Minus,
                         Divide,
                         Times,
                         Int "5",
                         Semicolon,
                         Int "5",
                         Lessthan,
                         Int "10",
                         Greaterthan,
                         Int "5",
                         Semicolon,
                         Eof
                       ]
