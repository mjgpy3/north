{-# LANGUAGE DeriveFunctor #-}

module North.Parse.SourceLocation
    ( SourceLocation (..)
    ) where

data SourceLocation a = SourceLocation {
  line :: Int
  , column :: Int
  , located :: a
  }
  deriving (Show, Eq, Functor)
