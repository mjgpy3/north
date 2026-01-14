{-# LANGUAGE OverloadedStrings #-}

module North.BuiltIns.Effects (builtInEffects) where

import qualified Data.Text as T
import North.Eval.DescribedFactor
import North.Eval.Effects
import North.Types
import North.Types.SloppySignatures

builtInEffects :: [(T.Text, DescribedFactor Effect)]
builtInEffects =
    [
        ( "cr"
        , DescribedFactor
            { factorDescription = "Print a newline character"
            , factorSloppySignature =
                SloppySignature
                    { stackIn = []
                    , stackOut = []
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = Cr
            }
        )
    ,
        ( "say"
        , DescribedFactor
            { factorDescription = "Print the string on top of the stack"
            , factorSloppySignature =
                SloppySignature
                    { stackIn = [TString]
                    , stackOut = []
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = PrintString
            }
        )
    ,
        ( "."
        , DescribedFactor
            { factorDescription = "Print whatever is on top of the stack"
            , factorSloppySignature =
                SloppySignature
                    { stackIn = [TVar 'a']
                    , stackOut = []
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = Print
            }
        )
    ,
        ( "read-file"
        , DescribedFactor
            { factorDescription = "Read the file whose name is at the top of the stack"
            , factorSloppySignature =
                SloppySignature
                    { stackIn = [TString]
                    , stackOut = [TString]
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = ReadFile
            }
        )
    ,
        ( "trace"
        , DescribedFactor
            { factorDescription = "Show (without popping) the current item on top of the stack. Does not fail on empty stack."
            , factorSloppySignature =
                SloppySignature
                    { stackIn = []
                    , stackOut = []
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = Trace
            }
        )
    ,
        ( "trace-stack"
        , DescribedFactor
            { factorDescription = "Show (without popping) the ENTIRE stack (use with caution!)"
            , factorSloppySignature =
                SloppySignature
                    { stackIn = []
                    , stackOut = []
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = TraceStack
            }
        )
    ,
        ( "assert"
        , DescribedFactor
            { factorDescription = "Fail unless true is at the top of the stack"
            , factorSloppySignature =
                SloppySignature
                    { stackIn = [TBool]
                    , stackOut = []
                    , equivalentPatterns = Nothing
                    }
            , factorDefinition = Assert
            }
        )
    ]
