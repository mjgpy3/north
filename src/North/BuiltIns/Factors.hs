{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module North.BuiltIns.Factors (builtInFactors) where

import North.Eval.Env
import North.Types.TypeOf (typeOf)
import North.Eval.Errors (EvalError(..))
import qualified Data.Text as T
import North.Types.SloppySignatures
import North.Eval.DescribedFactor
import North.Types
import North.Values
import Data.Bifunctor (second)

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
   ),
   ("+", DescribedFactor {
            factorDescription = "Add two numbers"
          , factorSloppySignature = SloppySignature {
              stackIn=[TNum]
            , stackOut=[TNum, TNum]
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', b) <- pop envState'
              case (a, b) of
                (NInt n1, NInt n2) -> pure $ push (NInt $ n1 + n2) envState''
                (NFloat n1, NFloat n2) -> pure $ push (NFloat $ n1 + n2) envState''
                (NInt n1, NFloat n2) -> pure $ push (NFloat $ fromIntegral n1 + n2) envState''
                (NFloat n1, NInt n2) -> pure $ push (NFloat $ n1 + fromIntegral n2) envState''
                (NInt _, nonNumber) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
                (NFloat _, nonNumber) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
                (nonNumber, _) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
          }
   ),
   ("*", DescribedFactor {
            factorDescription = "Multiply two numbers"
          , factorSloppySignature = SloppySignature {
              stackIn=[TNum]
            , stackOut=[TNum, TNum]
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', b) <- pop envState'
              case (a, b) of
                (NInt n1, NInt n2) -> pure $ push (NInt $ n1 * n2) envState''
                (NFloat n1, NFloat n2) -> pure $ push (NFloat $ n1 * n2) envState''
                (NInt n1, NFloat n2) -> pure $ push (NFloat $ fromIntegral n1 * n2) envState''
                (NFloat n1, NInt n2) -> pure $ push (NFloat $ n1 * fromIntegral n2) envState''
                (NInt _, nonNumber) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
                (NFloat _, nonNumber) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
                (nonNumber, _) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
          }
   ),
   ("=", DescribedFactor {
            factorDescription = "Test if two values are equal"
          , factorSloppySignature = SloppySignature {
              stackIn=[TVar 'a', TVar 'b']
            , stackOut=[TBool]
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', b) <- pop envState'
              pure $ push (NBool $ a == b) envState''
          }
   )
   ]
