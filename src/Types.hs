module Types (Fragment(..)) where

-- generic type used for differentiating between text and code fragments at many points in the compilation pipeline
data Fragment a b = TextFragment a | CodeFragment b
    deriving (Show, Eq)