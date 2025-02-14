module North.Parse
    ( tokenize
    ) where

import qualified Data.Text as T
import North.Parse.Tokens
import qualified Data.Char as Char
import qualified Data.List as List
import Safe (readMay)

tokenize :: T.Text -> [Token]
tokenize = concatMap (uncurry $ tokenizeLine 0) . zip [0..]  . T.lines

tokenizeLine :: Int -> Int -> T.Text -> [Token]
tokenizeLine c l t =
  case T.uncons t of
    Nothing -> []
    Just (chr, rest) ->
      case List.find (\(isSpecial, _) -> isSpecial chr) special of
        Just (_, mkToken) -> Token {line=l, column=c, tok=mkToken chr}:tokenizeLine (c+1) l rest
        Nothing ->
          let
            (term, rest') = T.break (\chr' -> any (\(isSpecial, _) -> isSpecial chr') special) t
          in
            case readMay (T.unpack term) of
              Just num -> Token {line=l, column=c, tok=NumberLiteral num}:tokenizeLine (c+T.length term) l rest'
              Nothing -> Token {line=l, column=c, tok=Ident term}:tokenizeLine (c+T.length term) l rest'
  where
    special = [ ((== '('), const OpenParen)
              , ((== ')'), const CloseParen)
              , ((== '\\'), const Backslash)
              , ((== ':'), const Colon)
              , ((== ';'), const Semicolon)
              , (Char.isSpace, Whitespace)
              ]
