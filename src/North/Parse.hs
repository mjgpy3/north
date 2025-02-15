module North.Parse
    ( parse
    ) where

import qualified Data.Text as T
import North.TopLevelTerms
import North.Parse.Tokenize
import North.Parse.Comments
import North.Parse.TokenParser
import North.Parse.Whitespace
import North.Parse.Errors (ParseError)

parse :: T.Text -> Either ParseError [TopLevelTerm]
parse =
  parseTokens .
  eraseWhitespace .
  eraseLineComments .
  eraseBlockComments .
  -- TODO compact out strings.
  tokenize
