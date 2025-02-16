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
import Data.List ((!?))

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
   ("drop", DescribedFactor {
            factorDescription = "Drop top element"
          , factorSloppySignature = SloppySignature {
              stackIn=[TVar 'a']
            , stackOut=[]
            }
          , factorDefinition = \envState ->
              case pop envState of
                Left err -> (envState, Left err)
                Right (envState', _) -> (envState', Right Unit)
          }
   ),
   ("rot", DescribedFactor {
            factorDescription = "Rotate the top three stack entries"
          , factorSloppySignature = SloppySignature {
              stackIn=[TVar 'a', TVar 'b', TVar 'c']
            , stackOut=[TVar 'c', TVar 'b', TVar 'a']
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', b) <- pop envState'
              (envState''', c) <- pop envState''
              pure $ push c $ push b $ push a $ envState'''
          }
   ),
   ("nip", DescribedFactor {
            factorDescription = "Removes the item directly below the top"
          , factorSloppySignature = SloppySignature {
              stackIn=[TVar 'a', TVar 'b']
            , stackOut=[TVar 'a']
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', _) <- pop envState'
              pure $ push a $ envState''
          }
   ),
   ("tuck", DescribedFactor {
            factorDescription = "Duplicates the item directly below the top on top"
          , factorSloppySignature = SloppySignature {
              stackIn=[TVar 'a', TVar 'b']
            , stackOut=[TVar 'b', TVar 'a', TVar 'b']
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', b) <- pop envState'
              pure $ push b $ push a $ push b $ envState''
          }
   ),
   ("pick", DescribedFactor {
            factorDescription = "Duplicates an item by index. The top indicates the index of the item below (zero-based) to duplicate"
          , factorSloppySignature = SloppySignature {
              stackIn=[TNum, TVar 'a', TVar 'b', TVar 'c']
            , stackOut=[TVar 'd']
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState'@EnvState {stack=st}, a) <- pop envState
              case a of
                NInt i ->
                  case st !? fromIntegral i of
                    Nothing -> Left StackUnderflow
                    Just v -> pure $ push v envState'
                nonNumber -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
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
   ("-", DescribedFactor {
            factorDescription = "Subtract top from next"
          , factorSloppySignature = SloppySignature {
              stackIn=[TNum]
            , stackOut=[TNum, TNum]
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              (envState'', b) <- pop envState'
              case (a, b) of
                (NInt n1, NInt n2) -> pure $ push (NInt $ n2 - n1) envState''
                (NFloat n1, NFloat n2) -> pure $ push (NFloat $ n2 - n1) envState''
                (NInt n1, NFloat n2) -> pure $ push (NFloat $ n2 - fromIntegral n1) envState''
                (NFloat n1, NInt n2) -> pure $ push (NFloat $ fromIntegral n2 - n1) envState''
                (NInt _, nonNumber) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
                (NFloat _, nonNumber) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
                (nonNumber, _) -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
          }
   ),
   ("incr", DescribedFactor {
            factorDescription = "Increment top number"
          , factorSloppySignature = SloppySignature {
              stackIn=[TNum]
            , stackOut=[TNum]
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              case a of
                NInt n1 -> pure $ push (NInt $ n1 + 1) envState'
                NFloat n1 -> pure $ push (NFloat $ n1 + 1) envState'
                nonNumber -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
          }
   ),
   ("decr", DescribedFactor {
            factorDescription = "Decrement top number"
          , factorSloppySignature = SloppySignature {
              stackIn=[TNum]
            , stackOut=[TNum]
            }
          , factorDefinition = \envState -> either (second Left . (envState, )) ((, Right Unit)) $ do
              (envState', a) <- pop envState
              case a of
                NInt n1 -> pure $ push (NInt $ n1 - 1) envState'
                NFloat n1 -> pure $ push (NFloat $ n1 - 1) envState'
                nonNumber -> Left $ TNum `TypeExpectedButGot'` (typeOf nonNumber, nonNumber)
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
