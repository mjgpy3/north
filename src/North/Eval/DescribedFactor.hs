module North.Eval.DescribedFactor (DescribedFactor(..)) where

import North.Types.SloppySignatures
import qualified Data.Text as T

data DescribedFactor a = DescribedFactor  {
   factorDescription :: T.Text
   , factorSloppySignature :: SloppySignature
   , factorDefinition :: a
   }
