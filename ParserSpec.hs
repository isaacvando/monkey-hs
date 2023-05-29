import Lexer (lex)
import Parser
import Test.Hspec
import Text.RawString.QQ (r)

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    it "numerical let expression" $ do
      let tokens = Lexer.lex "let beep_boop = 789987;"
      parse tokens `shouldBe` [Let "beep_boop" (Int 789987)]