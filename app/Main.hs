module Main (main) where

import Lib (compile)
import Options.Applicative
import Types (CLIOptions (..))

import Data.Version (showVersion)
import Paths_nothing_but_math (version)

-- TODO: make release on GitHub
-- TODO: add ability to embed operator/syntax/grammar overview/table in output LaTeX (for Appendices)

main :: IO ()
main = compile =<< execParser opts
  where
    opts = info (cliParser <**> helper <**> versionOption)
        ( fullDesc
        <> progDesc "Compile an NBM source file to Haskell and LaTeX. Optionally generates a PDF"
        <> header "nbm - Nothing But Math compiler" )

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version)
    ( long "version"
    <> short 'v'
    <> help "Show the compiler version" )

cliParser :: Parser CLIOptions
cliParser = CLIOptions
    <$> argument str (metavar "PATHNAME" <> help "The path to your .nbm file")
    <*> strOption 
        ( long "out-dir" 
        <> short 'o' 
        <> metavar "DIR" 
        <> value "." -- default to the current directory
        <> showDefault 
        <> help "Output directory for generated files" )
    <*> switch
        ( long "pdf"
        <> short 'p'
        <> help "Runs pdflatex to generate a PDF document" )
    <*> switch
        ( long "wrapdoc"
        <> short 'w'
        <> help "Wraps the LaTeX output for basic PDF document output. Do not use this if you are using a LaTeX template" )
    <*> strOption 
        ( long "module-name" 
        <> short 'm' 
        <> metavar "MODULE_NAME" 
        <> value "NBM" -- the default Haskell module name will be "NBM"
        <> showDefault 
        <> help "Module name of the generated Haskell library" )