{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import North
import North.Parse
import North.Eval
import Data.Foldable (for_)
import North.BuiltIns
import North.Eval
import qualified System.Environment as Env
import qualified Data.Text.IO as TIO

main :: IO ()
main =
  Env.getArgs >>= \case
    [] -> print (parse ": FOO (hi -- there)\n  DUP [a,b,c]?->[a,*] []? * DUP 42 \"Here is a string with escaped quotes \"\"\";")
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
