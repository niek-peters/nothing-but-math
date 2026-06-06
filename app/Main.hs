module Main (main) where

import Lib (compile)
import Options.Applicative
import Types (CLIOptions (..))
import Language.Haskell.Interpreter (loadModules, setTopLevelModules, as, interpret, runInterpreter)
import CodeGenHaskell (codeGenExpr)
import Parser (runExprParser)
import Elab (Scope, elabTopLevelExpr)

main :: IO ()
main = compile =<< execParser opts
  where
    opts = info (cliParser <**> helper)
        ( fullDesc
        <> progDesc "Compile an NBM source file to Haskell and LaTeX. Optionally compiles to PDF"
        <> header "nbm - Nothing But Math compiler" )

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
        <> help "Wraps the LaTeX output for basic PDF document output" )
    <*> strOption 
        ( long "module-name" 
        <> short 'm' 
        <> metavar "MODULE_NAME" 
        <> value "NBM" -- the default Haskell module name will be "NBM"
        <> showDefault 
        <> help "Module name of the generated Haskell library" )

-- runCompiler :: Options -> IO ()
-- runCompiler (Options file dir toPDF wrapDoc) = compile file dir toPDF wrapDoc
