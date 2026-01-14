module North.Types.SloppySignatures (SloppySignature (..)) where

import North.Types
import North.Values

data SloppySignature = SloppySignature
    { stackIn :: [Type]
    , stackOut :: [Type]
    , equivalentPatterns :: Maybe [TransformCheck]
    }
