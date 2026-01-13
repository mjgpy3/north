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
import North.TopLevelTerms
import North.Values
import Test.Hspec

spec :: Spec
spec =
    describe "parseTokens" $ do
        context "parsing pattern terms" $ do
            let
                validStackPatternExamples =
                    [ ("[]", StackPattern [] False)
                    , ("[*]", StackPattern [] True)
                    , ("[a]", StackPattern "a" False)
                    , ("[a,*]", StackPattern "a" True)
                    , ("[a,b,c,*]", StackPattern "abc" True)
                    , ("[a,b,c]", StackPattern "abc" False)
                    -- Commas are optional!
                    , ("[abc]", StackPattern "abc" False)
                    -- Commas are optional (so we can have as many as we want)!
                    , ("[,,,,a,,b,c,,,,,]", StackPattern "abc" False)
                    ]

            for_ validStackPatternExamples $ \(sp, expected) -> do
                let ex = sp <> "?"

                successCase ex $ Check expected

                casesThatBreak ex

            for_ validStackPatternExamples $ \(sp1, expected1) ->
                for_ validStackPatternExamples $ \(sp2, expected2) -> do
                    let txExample = sp1 <> "->" <> sp2

                    successCase txExample $ Transform expected1 expected2

                    casesThatBreak txExample

                    let ctxExample = sp1 <> "?->" <> sp2

                    successCase ctxExample $ CheckedTransform expected1 expected2

                    casesThatBreak ctxExample
  where
    successCase example expected =
        it ("parses " <> show expected <> " from " <> unpack example) $ do
            parseTokens [term example] `shouldBe` Right [TopLevelValue $ SourceLocation 0 0 $ Pattern expected]

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
