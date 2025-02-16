{-# LANGUAGE OverloadedStrings #-}

module North.BuiltIns (envWithBuiltIns) where

import North.Eval.Env
import qualified Data.Map.Strict as Map
import North.Eval.Effects
import North.Types.SloppySignatures
import North.Eval.DescribedFactor
import North.Types

envWithBuiltIns :: Env
envWithBuiltIns = State emptyEnvState {
    factors = Map.empty
  , constants = Map.empty
  , variables = Map.empty
  , effects = Map.fromList [
      ("cr", DescribedFactor {
               factorDescription = "Print a newline character"
             , factorSloppySignature = SloppySignature {
                 stackIn=[]
               , stackOut=[]
               }
             , factorDefinition = Cr
             }
      ),
      ("say", DescribedFactor {
               factorDescription = "Print the string on top of the stack"
             , factorSloppySignature = SloppySignature {
                 stackIn=[TString]
               , stackOut=[]
               }
             , factorDefinition = PrintString
             }
      ),
      ("read-file", DescribedFactor {
               factorDescription = "Read the file whose name is at the top of the stack"
             , factorSloppySignature = SloppySignature {
                 stackIn=[TString]
               , stackOut=[TString]
               }
             , factorDefinition = ReadFile
             }
      )]
  }

--  -- Print a newline
--  = Cr
--  -- Read a file
--  | ReadFile
--  -- Pop and show whatever's at the top of the stack
--  | Print
--  -- Pop and show the string at the top of the stack
--  | PrintString
