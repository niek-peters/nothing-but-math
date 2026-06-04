module Types (CLIOptions(..), Fragment(..)) where

data CLIOptions = CLIOptions { filePath :: FilePath, outDir :: FilePath, toPDF :: Bool, wrapDoc :: Bool, moduleName :: String }
  deriving (Show, Eq)

-- generic type used for differentiating between text and code fragments at many points in the compilation pipeline
data Fragment a b = TextFragment a | CodeFragment b
    deriving (Show, Eq)