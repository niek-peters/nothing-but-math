-- | Common shared types for CLI options and fragment wrappers.
module Types (module Types) where   -- export everything

-- | Command-line options controlling compilation input, output, and rendering.
data CLIOptions = CLIOptions { filePath :: FilePath, outDir :: FilePath, toPDF :: Bool, wrapDoc :: Bool, moduleName :: String }
  deriving (Show, Eq)

-- | Type representing text, definition, or evaluation fragments across the pipeline.
data Fragment a b c = TextFragment a | DefinitionFragment b | EvalFragment c
    deriving (Show, Eq)