{-# LANGUAGE LambdaCase #-}
module North.Parse.TokenParser
    ( parseTokens
    ) where

import North.Values
import North.Parse.SourceLocation
import North.TopLevelTerms
import North.Parse.ParsableTerms
import North.Parse.Errors (ParseError(..))

-- TODO accumulate errors
parseTokens :: [ParsableTerm] -> Either ParseError [TopLevelTerm]
parseTokens = \case
  -- EOF
  [] -> Right []

  -- Var def: `var foo`
  (ParsableTerm SourceLocation{located=VarTerm}:ParsableTerm loc@SourceLocation{located=IdentTerm name}:rest) -> do
    defs <- parseTokens rest
    Right $ (VarDef $ fmap (const name) loc):defs

  -- Var without name: `var 42`
  (ParsableTerm SourceLocation{located=VarTerm}:nonIdent:_) ->
    Left $ ExpectedNameButGot nonIdent

  -- Var without anything else: `var`
  loneVar@[ParsableTerm SourceLocation{located=VarTerm}] ->
    Left $ IncompleteTopLevelTerm loneVar

  -- Const def: `const answer 42`, also handles non values in value position
  (ParsableTerm SourceLocation{located=ConstTerm}:ParsableTerm loc@SourceLocation{located=IdentTerm name}:valueTok:rest) -> do
    value <- parseValue valueTok
    defs <- parseTokens rest
    Right $ (ConstDef (fmap (const name) loc) value):defs

  -- Const without name: `const 42 42`
  (ParsableTerm SourceLocation{located=ConstTerm}:nonIdent:_:_) -> do
    Left $ ExpectedNameButGot nonIdent

  -- Incomplete Const 1: `const 42`
  constPlus@[ParsableTerm SourceLocation{located=ConstTerm}, _] -> do
    Left $ IncompleteTopLevelTerm constPlus

  -- Const without anything else: `const`
  loneConst@[ParsableTerm SourceLocation{located=ConstTerm}] ->
    Left $ IncompleteTopLevelTerm loneConst

  -- This should be handled elsewhere
  (bad@(ParsableTerm SourceLocation{located=SemicolonTerm}):_) ->
    Left $ CompilerErrorShouldNotExistAtThisStage bad

  -- Factor: `: foo <values> ;`
  (ParsableTerm SourceLocation{located=ColonTerm}:nameTok@(ParsableTerm loc@SourceLocation{located=IdentTerm name}):rest) ->
    let
      (valueToks, hopefullySemi) = break ((== SemicolonTerm) . located . unTerm) rest
    in
      case hopefullySemi of
        (ParsableTerm SourceLocation{located=SemicolonTerm}:rest') -> do
          values <- traverse parseValue valueToks
          defs <- parseTokens rest'
          Right $ (WordDef (fmap (const name) loc) values):defs
        _ -> Left $ UnterminatedFactorDef nameTok

  -- Factor without name: `: foo 42`
  (ParsableTerm SourceLocation{located=ColonTerm}:nonIdent:_) ->
    Left $ ExpectedNameButGot nonIdent

  -- Incomplete factor: `:`
  incompl@[ParsableTerm SourceLocation{located=ColonTerm}] -> do
    Left $ IncompleteTopLevelTerm incompl

  -- Top-level int
  (valueTok@(ParsableTerm SourceLocation{located=IntTerm _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs

  -- Top-level float
  (valueTok@(ParsableTerm SourceLocation{located=FloatTerm _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs

  -- Top-level strinng
  (valueTok@(ParsableTerm SourceLocation{located=StringTerm _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs

  -- Top-level word
  (valueTok@(ParsableTerm SourceLocation{located=IdentTerm _}):rest) -> do
    defs <- parseTokens rest
    value <- parseValue valueTok
    Right $ (TopLevelValue value):defs

parseValue :: ParsableTerm -> Either ParseError (SourceLocation Value)
parseValue fullTok@(ParsableTerm loc@SourceLocation{located=tok} ) =
  case tok of
    IdentTerm name -> Right $ const (Word name) <$> loc
    IntTerm v -> Right $ const (NInt v) <$> loc
    FloatTerm v -> Right $ const (NFloat v) <$> loc
    StringTerm v -> Right $ const (NString v) <$> loc
    _ -> Left $ ExpectedValueButGot fullTok
