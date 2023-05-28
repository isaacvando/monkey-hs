import Lexer
import Text.RawString.QQ

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "lexer" $ do
        it "lexes symbols" $ do
            Lexer.lex "=+(){},;" `shouldBe` [Assign, Plus, Lparen, Rparen, Lbrace, Rbrace, Comma, Semicolon]
            
        it "lexes a full code block" $ do
            let 
                input =  [r|=|] 
            in 
                Lexer.lex input `shouldBe` [Assign]
