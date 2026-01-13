module North.Parse (
    parse,
) where

import qualified Data.Text as T
import North.Parse.Errors (ParseError)
import North.Parse.Sections
import North.Parse.TokenParser
import North.Parse.Tokenize
import North.TopLevelTerms

parse :: T.Text -> Either ParseError [TopLevelTerm]
parse =
    parseTokens
        . parseSections
        . tokenize
