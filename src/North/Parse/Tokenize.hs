{-# LANGUAGE OverloadedStrings #-}

module North.Parse.Tokenize
    ( tokenize
    ) where

import qualified Data.Text as T
import North.Parse.Tokens
import North.Parse.SourceLocation
import qualified Data.Char as Char
import qualified Data.List as List
import Safe (readMay)

tokenize :: T.Text -> [Token]
tokenize = concatMap (uncurry $ tokenizeLine 0) . zip [0..]  . T.lines

tokenizeLine :: Int -> Int -> T.Text -> [Token]
tokenizeLine c l t =
  case T.uncons t of
    Nothing -> [loc $ Whitespace '\n']
    Just (chr, rest) ->
      case List.find (isSpecial chr) special of
        Just (_, mkToken) -> loc (mkToken chr):tokenizeLine (c+1) l rest
        Nothing ->
          let
            (term, rest') = T.break ((`any` special) . isSpecial) t
            continue tok' = loc tok':tokenizeLine (c+T.length term) l rest'
          in
            case (readMay (T.unpack term), readMay (T.unpack term)) of
              (Just num, _) -> continue $ IntLiteral num
              (_, Just num) -> continue $ FloatLiteral num
              _ ->
                case term of
                  "var" -> continue Var
                  "const" -> continue Const
                  "true" -> continue TrueTok
                  "false" -> continue FalseTok
                  _ -> continue $ Ident term
  where
    loc tok' = Token SourceLocation {column = c, line = l, located=tok'}
    isSpecial chr (isSpecial', _) = isSpecial' chr
    special = [ ((== '('), const OpenParen)
              , ((== ')'), const CloseParen)
              , ((== '\\'), const Backslash)
              , ((== ':'), const Colon)
              , ((== ';'), const Semicolon)
              , ((== '"'), const DoubleQuote)
              , (Char.isSpace, Whitespace)
              ]
