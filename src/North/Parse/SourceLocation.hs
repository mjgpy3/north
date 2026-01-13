{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module North.Parse.SourceLocation (
    SourceLocation (..),
    formatLineColumn,
) where

import qualified Data.Text as T

data SourceLocation a = SourceLocation
    { line :: Int
    , column :: Int
    , located :: a
    }
    deriving (Show, Eq, Functor)

formatLineColumn :: SourceLocation a -> T.Text
formatLineColumn SourceLocation{line = l, column = c} =
    T.concat [T.pack $ show l, ":", T.pack $ show c, ": "]
