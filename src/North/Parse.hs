module North.Parse
    ( parse
    ) where

import qualified Data.Text as T
import North.TopLevelTerms
import North.Parse.Tokenize
import North.Parse.Sections
import North.Parse.TokenParser
import North.Parse.Errors (ParseError)

parse :: T.Text -> Either ParseError [TopLevelTerm]
parse =
  parseTokens .
  parseSections .
  tokenize
