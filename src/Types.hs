module Types (module Types) where   -- export everything

data CLIOptions = CLIOptions { filePath :: FilePath, outDir :: FilePath, toPDF :: Bool, wrapDoc :: Bool, moduleName :: String }
  deriving (Show, Eq)

-- generic type used for differentiating between text, definition and eval fragments at many points in the compilation pipeline
data Fragment a b c = TextFragment a | DefinitionFragment b | EvalFragment c
    deriving (Show, Eq)