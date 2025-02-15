{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module North.Parse.Sections
    ( parseSections
    ) where

import North.Parse.Tokens
import North.Parse.SourceLocation
import North.Parse.ParsableTerms
import qualified Data.Text as T

parseSections :: [Token] -> [ParsableTerm]
parseSections = \case
  [] -> []

  Token loc@(SourceLocation {located=DoubleQuote }):rest ->
    let
      (stringInner, rest') = breakAndSquashQuotes rest
    in
      ParsableTerm (fmap (const $ StringTerm $ rebuildText stringInner) loc):parseSections rest'

  Token (SourceLocation {located=OpenParen }):rest ->
    let
      (_, rest') = break ((== CloseParen) . located . unToken) rest
    in
      parseSections (drop 1 rest')
  Token (SourceLocation {located=Backslash }):rest ->
    let
      (_, rest') = break ((== Whitespace '\n') . located . unToken) rest
    in
      parseSections rest'


  Token loc@(SourceLocation {located=Colon }):rest ->
    ParsableTerm (fmap (const ColonTerm) loc):parseSections rest
  Token loc@(SourceLocation {located=Semicolon }):rest ->
    ParsableTerm (fmap (const SemicolonTerm) loc):parseSections rest
  Token loc@(SourceLocation {located=Ident n }):rest ->
    ParsableTerm (fmap (const $ IdentTerm n) loc):parseSections rest
  Token loc@(SourceLocation {located=IntLiteral n }):rest ->
    ParsableTerm (fmap (const $ IntTerm n) loc):parseSections rest
  Token loc@(SourceLocation {located=FloatLiteral n }):rest ->
    ParsableTerm (fmap (const $ FloatTerm n) loc):parseSections rest
  Token loc@(SourceLocation {located=Var}):rest ->
    ParsableTerm (fmap (const $ VarTerm) loc):parseSections rest
  Token loc@(SourceLocation {located=Const}):rest ->
    ParsableTerm (fmap (const $ ConstTerm) loc):parseSections rest

  -- TODO should error
  Token SourceLocation {located=CloseParen}:rest -> parseSections rest

  Token SourceLocation {located=Whitespace _}:rest -> parseSections rest

-- Squashed out escaped double quotes (e.g. "" within doubles), removes final double quote
breakAndSquashQuotes :: [Token] -> ([Token], [Token])
breakAndSquashQuotes = \case
  [] ->
    ([], [])

  [Token (SourceLocation {located=DoubleQuote })] ->
    ([], [])

  (Token (SourceLocation {located=DoubleQuote }):q@(Token (SourceLocation {located=DoubleQuote })):rest) ->
    let
     (l, r) = breakAndSquashQuotes rest
    in
      (q:l, r)

  (Token (SourceLocation {located=DoubleQuote }):rest) ->
    ([], rest)

  (tok:rest) ->
    let
     (l, r) = breakAndSquashQuotes rest
    in
      (tok:l, r)

-- Can be used for strings if the escaped double quotes are squashed out
rebuildText :: [Token] -> T.Text
rebuildText =
  T.concat . fmap (go . located . unToken)

  where
    go = \case
      Ident i -> i
      OpenParen -> "("
      CloseParen -> ")"
      Backslash -> "\\"
      Colon -> ":"
      Semicolon -> ";"
      Whitespace c -> T.singleton c
      IntLiteral n -> T.pack $ show n
      FloatLiteral n -> T.pack $ show n
      DoubleQuote -> "\""
      Var -> "var"
      Const -> "const"
