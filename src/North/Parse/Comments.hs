{-# LANGUAGE LambdaCase #-}
module North.Parse.Comments
    ( eraseBlockComments
    , eraseLineComments
    ) where

import North.Parse.Tokens
import North.Parse.SourceLocation

eraseBlockComments :: [Token] -> [Token]
eraseBlockComments = \case
  [] -> []
  Token (SourceLocation {located=OpenParen }):rest -> eraseBlockComments $ drop 1 $ dropWhile ((/= CloseParen) . located . unToken) rest
  (okTok:rest) -> okTok:eraseBlockComments rest

eraseLineComments :: [Token] -> [Token]
eraseLineComments = \case
  [] -> []
  Token (SourceLocation {located=Backslash }):rest -> eraseBlockComments $ dropWhile ((/= Whitespace '\n') . located . unToken) rest
  (okTok:rest) -> okTok:eraseBlockComments rest
