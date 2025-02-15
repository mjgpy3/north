{-# LANGUAGE LambdaCase #-}
module North.Parse.Whitespace
    ( eraseWhitespace
    ) where

import North.Parse.Tokens
import North.Parse.SourceLocation

eraseWhitespace :: [Token] -> [Token]
eraseWhitespace = filter (not . isWhitespace)
  where
    isWhitespace = \case
       Token (SourceLocation {located=Whitespace _ }) -> True
       _ -> False
