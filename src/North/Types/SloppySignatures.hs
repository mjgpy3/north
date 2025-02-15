
module North.Types.SloppySignatures (SloppySignature(..)) where

import North.Types

data SloppySignature = SloppySignature {
  typeStackIn :: [Type]
  , typeStackOut :: [Type]
  }
