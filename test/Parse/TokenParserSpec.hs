{-# LANGUAGE OverloadedStrings #-}

module Parse.TokenParserSpec where

import Data.Either (isLeft, isRight)
import Data.Foldable (for_)
import Data.Text (unpack)
import qualified Data.Text as T
import North.Parse.ParsableTerms
import North.Parse.SourceLocation
import North.Parse.TokenParser
import North.Parse.Tokenize
import North.Parse.Tokens
import Test.Hspec

spec :: Spec
spec =
    describe "parseTokens" $ do
        context "parsing pattern terms" $ do
            let
                validStackPatternExamples =
                    [ "[]"
                    , "[*]"
                    , "[a]"
                    , "[a,*]"
                    , "[a,b,c,*]"
                    , "[a,b,c]"
                    ]

            for_ validStackPatternExamples $ \sp -> do
                let example = sp <> "?"

                it ("parses " <> unpack example) $ do
                    parseTokens [term example] `shouldSatisfy` isRight

                casesThatBreak example

            for_ validStackPatternExamples $ \sp1 ->
                for_ validStackPatternExamples $ \sp2 -> do
                    let txExample = sp1 <> "->" <> sp2

                    it ("parses " <> unpack txExample) $ do
                        parseTokens [term txExample] `shouldSatisfy` isRight

                    casesThatBreak txExample

                    let ctxExample = sp1 <> "?->" <> sp2

                    it ("parses " <> unpack ctxExample) $ do
                        parseTokens [term ctxExample] `shouldSatisfy` isRight

                    casesThatBreak ctxExample
  where
    casesThatBreak otherwiseGoodExample = do
        it ("fails to parse " <> unpack otherwiseGoodExample <> " when it has extraneous text at the end") $ do
            for_ ["a", ".", "?", "->", ","] $ \extra ->
                parseTokens [term $ otherwiseGoodExample <> extra] `shouldSatisfy` isLeft

        it ("fails to parse " <> unpack otherwiseGoodExample <> " when it's missing any of its brackets") $ do
            let (left, right) = T.breakOn "->" otherwiseGoodExample

            let
                leftExamples =
                    [ T.replace "[" "" left <> right
                    , T.replace "]" "" left <> right
                    ]

                rightExamples =
                    filter
                        (const $ right /= "")
                        [ left <> T.replace "[" "" right
                        , left <> T.replace "]" "" right
                        ]

            for_ (leftExamples <> rightExamples) $ \exampleWithoutBracket -> do
                parseTokens [term exampleWithoutBracket] `shouldSatisfy` isLeft

        it ("fails to parse " <> unpack otherwiseGoodExample <> " when it's got a bad operator") $ do
            parseTokens [term $ T.replace "?" "+" $ T.replace "->" "*" otherwiseGoodExample] `shouldSatisfy` isLeft

        it ("fails to parse " <> unpack otherwiseGoodExample <> " when it's got a bad character inside") $ do
            let (left, right) = T.breakOn "]" otherwiseGoodExample
            parseTokens [term $ left <> "&" <> right] `shouldSatisfy` isLeft

    term = ParsableTerm . SourceLocation 0 0 . PatternTerm
