module North.Types.SloppySignatures (SloppySignature (..)) where

import North.Types

data SloppySignature = SloppySignature
    { stackIn :: [Type]
    , stackOut :: [Type]
    }
