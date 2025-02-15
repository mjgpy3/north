{-# LANGUAGE LambdaCase #-}
module North.Parse.TokenParser
    ( parseTokens
    ) where

import North.Values
import North.Parse.SourceLocation
import North.TopLevelTerms
import North.Parse.Tokens
import North.Parse.Errors (ParseError(..))
import Debug.Trace

-- TODO accumulate errors
parseTokens :: [Token] -> Either ParseError [TopLevelTerm]
parseTokens = \case
  -- EOF
  [] -> Right []

  -- Var def: `var foo`
  (Token SourceLocation{located=Var}:Token loc@SourceLocation{located=Ident name}:rest) -> do
    defs <- parseTokens rest
    Right $ (VarDef $ fmap (const name) loc):defs

  -- Var without name: `var 42`
  (Token SourceLocation{located=Var}:nonIdent:_) ->
    Left $ ExpectedNameButGot nonIdent

  -- Var without anything else: `var`
  loneVar@[Token SourceLocation{located=Var}] ->
    Left $ IncompleteTopLevelTerm loneVar

  -- Const def: `const answer 42`, also handles non values in value position
  (Token SourceLocation{located=Const}:Token loc@SourceLocation{located=Ident name}:valueTok:rest) -> do
    value <- parseValue valueTok
    defs <- parseTokens rest
    Right $ (ConstDef (fmap (const name) loc) value):defs

  -- Const without name: `const 42 42`
  (Token SourceLocation{located=Const}:nonIdent:_:_) -> do
    Left $ ExpectedNameButGot nonIdent

  -- Incomplete Const 1: `const 42`
  constPlus@[Token SourceLocation{located=Const}, _] -> do
    Left $ IncompleteTopLevelTerm constPlus

  -- Const without anything else: `const`
  loneConst@[Token SourceLocation{located=Const}] ->
    Left $ IncompleteTopLevelTerm loneConst

  -- These should all be handled elsewhere
  (bad@(Token SourceLocation{located=OpenParen}):_) ->
    Left $ CompilerErrorShouldNotExistAtThisStage bad
  (bad@(Token SourceLocation{located=CloseParen}):_) ->
    Left $ CompilerErrorShouldNotExistAtThisStage bad
  (bad@(Token SourceLocation{located=Backslash}):_) ->
    Left $ CompilerErrorShouldNotExistAtThisStage bad
  (bad@(Token SourceLocation{located=Whitespace _}):_) ->
    Left $ CompilerErrorShouldNotExistAtThisStage bad
  (bad@(Token SourceLocation{located=Semicolon}):_) ->
    Left $ CompilerErrorShouldNotExistAtThisStage bad

  -- Factor: `: foo <values> ;`
  (Token SourceLocation{located=Colon}:nameTok@(Token loc@SourceLocation{located=Ident name}):rest) ->
    let
      (valueToks, hopefullySemi) = break ((== Semicolon) . located . unToken) rest
    in
      case traceShow hopefullySemi  hopefullySemi of
        (Token SourceLocation{located=Semicolon}:rest') -> do
          values <- traverse parseValue valueToks
          defs <- parseTokens rest'
          Right $ (WordDef (fmap (const name) loc) values):defs
        _ -> Left $ UnterminatedFactorDef nameTok

  -- Factor without name: `: foo 42`
  (Token SourceLocation{located=Colon}:nonIdent:_) ->
    Left $ ExpectedNameButGot nonIdent

  -- Incomplete factor: `:`
  incompl@[Token SourceLocation{located=Colon}] -> do
    Left $ IncompleteTopLevelTerm incompl

  -- Top-level int
  (valueTok@(Token SourceLocation{located=IntLiteral _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs

  -- Top-level float
  (valueTok@(Token SourceLocation{located=FloatLiteral _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs

  -- Top-level word
  (valueTok@(Token SourceLocation{located=Ident _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs


parseValue :: Token -> Either ParseError (SourceLocation Value)
parseValue fullTok@( Token loc@SourceLocation{located=tok} ) =
  case tok of
    Ident name -> Right $ const (Word name) <$> loc
    IntLiteral v -> Right $ const (NInt v) <$> loc
    FloatLiteral v -> Right $ const (NFloat v) <$> loc
    _ -> Left $ ExpectedValueButGot fullTok
