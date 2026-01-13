module North.Eval.DescribedFactor (DescribedFactor (..)) where

import qualified Data.Text as T
import North.Types.SloppySignatures

data DescribedFactor a = DescribedFactor
    { factorDescription :: T.Text
    , factorSloppySignature :: SloppySignature
    , factorDefinition :: a
    }
