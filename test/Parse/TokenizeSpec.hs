{-# LANGUAGE OverloadedStrings #-}

module Parse.TokenizeSpec where

import North.Parse.SourceLocation
import North.Parse.Tokenize
import North.Parse.Tokens
import Test.Hspec

spec :: Spec
spec =
    describe "tokenize" $ do
        it "considers pattern transforms to be identifiers" $ do
            take 1 (tokenize "[]->[]") `shouldBe` [Token (SourceLocation{line = 0, column = 0, located = Ident "[]->[]"})]
            take 1 (tokenize "[a,b,*]->[a,*]") `shouldBe` [Token (SourceLocation{line = 0, column = 0, located = Ident "[a,b,*]->[a,*]"})]

        it "considers pattern checks to be identifiers" $ do
            take 1 (tokenize "[]?") `shouldBe` [Token (SourceLocation{line = 0, column = 0, located = Ident "[]?"})]
            take 1 (tokenize "[a,a]?") `shouldBe` [Token (SourceLocation{line = 0, column = 0, located = Ident "[a,a]?"})]

        it "considers checked pattern transforms to be identifiers" $ do
            take 1 (tokenize "[]?->[]") `shouldBe` [Token (SourceLocation{line = 0, column = 0, located = Ident "[]?->[]"})]
            take 1 (tokenize "[a,a,*]?->[a,*]") `shouldBe` [Token (SourceLocation{line = 0, column = 0, located = Ident "[a,a,*]?->[a,*]"})]
