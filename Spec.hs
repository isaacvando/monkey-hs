module Main where

import Test.Hspec

main :: IO ()
main = hspec $
    describe "hello" $
        it "works" $
            True `shouldBe` True
            