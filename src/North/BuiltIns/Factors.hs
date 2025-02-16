{-# LANGUAGE OverloadedStrings #-}

module North.BuiltIns.Factors (builtInFactors) where

import North.Eval.Env
import qualified Data.Text as T
import North.Types.SloppySignatures
import North.Eval.DescribedFactor
import North.Types
import North.Values

builtInFactors :: [(T.Text, DescribedFactor FactorDefinition)]
builtInFactors = [
   ("dup", DescribedFactor {
            factorDescription = "Duplicate the element at the top of the stack"
          , factorSloppySignature = SloppySignature {
              stackIn=[TVar 'a']
            , stackOut=[TVar 'a', TVar 'a']
            }
          , factorDefinition = \envState ->
              case pop envState of
                Left err -> (envState, Left err)
                Right (envState', v) -> (push v $ push v envState', Right Unit)
          }
   )
   ]
