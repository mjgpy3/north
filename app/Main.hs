{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import North.BuiltIns
import North.Eval
import North.Parse
import qualified System.Environment as Env

main :: IO ()
main =
    Env.getArgs >>= \case
        [file] -> do
            contents <- TIO.readFile file
            case parse contents of
                Left err -> do
                    putStrLn "Error Parsing: "
                    print err
                Right terms -> do
                    result <- evalMany envWithBuiltIns terms
                    case snd result of
                        Left err -> do
                            putStrLn "Error Evaluating: "
                            print err
                        Right _ok ->
                            pure ()
        _ -> error "Unexpected args, expected a single file"
