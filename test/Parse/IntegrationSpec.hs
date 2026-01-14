{-# LANGUAGE OverloadedStrings #-}

module Parse.IntegrationSpec (spec) where

import North.BuiltIns
import North.Eval
import North.Eval.Env
import North.Eval.Errors
import North.Parse
import North.Parse.SourceLocation
import North.Types
import North.Values
import Test.Hspec

spec :: Spec
spec =
    describe "integration tests" $ do
        describe "running" $ do
            describe "built-ins" $ do
                it "hello world" $
                    "\"Hello world\"" `shouldWorkAndResultInPeek` NString "Hello world"

                it "=" $ do
                    "1 1 =" `shouldWorkAndResultInPeek` NBool True
                    "1 2 =" `shouldWorkAndResultInPeek` NBool False

                it "not" $ do
                    "true not" `shouldWorkAndResultInPeek` NBool False
                    "false not" `shouldWorkAndResultInPeek` NBool True
                    "1 1 = not" `shouldWorkAndResultInPeek` NBool False

                it "dup" $
                    "42 dup" `shouldWorkAndResultInStack` [NInt 42, NInt 42]
                it "drop" $
                    "3 2 1 drop" `shouldWorkAndResultInStack` [NInt 2, NInt 3]

                it "rot" $
                    "4 3 2 1 rot" `shouldWorkAndResultInStack` [NInt 3, NInt 2, NInt 1, NInt 4]

                it "nip" $
                    "1 2 nip" `shouldWorkAndResultInStack` [NInt 2]

                it "tuck" $
                    "2 1 tuck" `shouldWorkAndResultInStack` [NInt 2, NInt 1, NInt 2]

                it "pick" $
                    "999 888 777 666 2 pick" `shouldWorkAndResultInStack` [NInt 888, NInt 666, NInt 777, NInt 888, NInt 999]

                it "+" $
                    "40 2 +" `shouldWorkAndResultInPeek` NInt 42

                it "-" $
                    "99 33 -" `shouldWorkAndResultInPeek` NInt 66

                it "incr" $
                    "99 incr" `shouldWorkAndResultInPeek` NInt 100

                it "decr" $
                    "36 decr" `shouldWorkAndResultInPeek` NInt 35

            describe "patterns" $ do
              it "empty stack check" $ do
                    "[]?" `shouldWorkAndResultInStack` [NBool True]
                    "[*]?" `shouldWorkAndResultInStack` [NBool True]

              it "non-empty stack check" $ do
                    "1 []?" `shouldWorkAndResultInStack` [NBool False, NInt 1]

              it "singleton stack check" $ do
                    "42 [a]?" `shouldWorkAndResultInStack` [NBool True, NInt 42]

              it "singleton stack with tail check" $ do
                    "42 [a*]?" `shouldWorkAndResultInStack` [NBool True, NInt 42]

              it "prolog patterns" $ do
                    "42 42 [aa]?" `shouldWorkAndResultInPeek` NBool True
                    "42 99 [aa]?" `shouldWorkAndResultInPeek` NBool False

              it "different names are okay" $ do
                    "42 42 [ab]?" `shouldWorkAndResultInPeek` NBool True

              it "empty with tail matches any" $ do
                    "42 42 [*]?" `shouldWorkAndResultInPeek` NBool True

              it "more complicated match" $ do
                    "1 2 1 1 [aaba]?" `shouldWorkAndResultInPeek` NBool True

              it "commas are optional" $ do
                    "1 2 1 1 [a,a,b,a]?" `shouldWorkAndResultInPeek` NBool True
                    "1 2 1 1 [,,,a,,ab,,,a,]?" `shouldWorkAndResultInPeek` NBool True

              it "drop 2" $ do
                    "4 3 2 1 [ab*]->[*]" `shouldWorkAndResultInStack` [NInt 3, NInt 4]

              it "bigger transform" $ do
                    "4 3 2 1 [abcd]->[ddacd]" `shouldWorkAndResultInStack` [NInt 4, NInt 4, NInt 1, NInt 3, NInt 4]

              it "tail preservation" $ do
                    "4 3 2 1 [a*]->[aa*]" `shouldWorkAndResultInStack` [NInt 1, NInt 1, NInt 2, NInt 3, NInt 4]

              it "checked without match (? means just skip it!)" $ do
                "42 [aa]?->[aaaaaaaaa]" `shouldWorkAndResultInStack` [NInt 42]

              it "drain" $ do
                "1 2 3 4 [*]->[]" `shouldWorkAndResultInStack` []

              it "bigger checked transform" $ do
                    "4 3 2 1 [abcd]?->[ddacd]" `shouldWorkAndResultInStack` [NInt 4, NInt 4, NInt 1, NInt 3, NInt 4]

              it "patterns can emulate equality" $ do
                    "1 1 =" `shouldWorkAndResultInStack` [NBool True]
                    "1 1 [aa*]? [abc*]->[a*]" `shouldWorkAndResultInStack` [NBool True]

                    "1 2 =" `shouldWorkAndResultInStack` [NBool False]
                    "1 2 [aa*]? [abc*]->[a*]" `shouldWorkAndResultInStack` [NBool False]

            describe "expected failures" $ do
                it "bad name in pattern" $
                    "1 2 [a*]->[b]" `shouldFailWithEvalError` PatternUnboundRhsName 'b'

                it "false assert" $
                    "false assert" `shouldFailWithEvalError` AssertionFailed

                it "no tail in pattern" $
                    "1 2 [a]->[b]" `shouldFailWithEvalError` InputPatternHasNoTailButMoreStackRemains

                it "pattern underflow" $
                    "1 [ab*]->[ab*]" `shouldFailWithEvalError` StackUnderflow

                it "underflow" $
                    "say" `shouldFailWithEvalError` StackUnderflow

                it "wrong type to plus" $
                    "\"foo\" 42 +" `shouldFailWithEvalError` TypeExpectedButGot' TNum (TString, NString "foo")

                it "fail prolog match on transform pattern" $
                    "1 2 [aa*]->[a]" `shouldFailWithEvalError` PatternMatchFailureNamesNotEqual 'a' (NInt 2) (NInt 1)
  where
    shouldFailWithEvalError code expected = do
        r <- freshRun code
        case r of
            Right (State _, Right{}) -> error "Expected eval error, got success"
            Right (RequestedEffect{}, _) -> error "Expected eval error, got requested effect"
            Left parseError -> error $ "Expected eval error, got parse error: " <> show parseError
            Right (_, Left (Located SourceLocation{located = evalError})) -> evalError `shouldBe` expected
            Right (_, Left evalError) -> evalError `shouldBe` expected

    shouldWorkAndResultInStack code expected = do
        r <- freshRun code
        case r of
            Right (State EnvState{stack = actual}, Right{}) -> actual `shouldBe` expected
            Right (RequestedEffect{}, _) -> error "Expected result, got requested effect"
            Left parseError -> error $ "Expected result, got parse error: " <> show parseError
            Right (_, Left evalError) -> error $ "Expected result, got eval error: " <> show evalError

    shouldWorkAndResultInPeek code expected = do
        r <- freshRun code
        case r of
            Right (State EnvState{stack = (v : _)}, _) -> v `shouldBe` expected
            Right (State EnvState{stack = []}, Right{}) -> error "Expected value atop stack, got empty stack"
            Right (RequestedEffect{}, _) -> error "Expected result, got requested effect"
            Left parseError -> error $ "Expected result, got parse error: " <> show parseError
            Right (_, Left evalError) -> error $ "Expected result, got eval error: " <> show evalError

    freshRun code =
        case parse code of
            Left err -> pure $ Left err
            Right terms -> Right <$> evalMany envWithBuiltIns terms
