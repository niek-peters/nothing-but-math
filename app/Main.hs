module Main (main) where

import Lib (compile)
import Options.Applicative

data Options = Options { filePath :: FilePath, outDir :: FilePath }
  deriving (Show, Eq)

main :: IO ()
main = runCompiler =<< execParser opts
  where
    opts = info (cliParser <**> helper)
        ( fullDesc
        <> progDesc "Compile an NBM source file to Haskell and LaTeX"
        <> header "nbm - Nothing But Math compiler" )

cliParser :: Parser Options
cliParser = Options
    <$> argument str (metavar "PATHNAME" <> help "The path to your .nbm file")
    <*> strOption 
        ( long "out-dir" 
        <> short 'o' 
        <> metavar "DIR" 
        <> value "." -- default to the current directory
        <> showDefault 
        <> help "Output directory for generated files" )

runCompiler :: Options -> IO ()
runCompiler (Options file dir) = compile file dir
