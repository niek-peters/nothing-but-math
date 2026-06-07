module Lib
    ( compile
    ) where
import Parser (parse, runExprParser)
import Text.Show.Pretty (pPrint)
import Elab (elab, Scope, elabTopLevelExpr, ElabResult)
import qualified CodeGenHaskell as CodeGenHaskell
import CodeGenHaskell (codeGenHaskell)
import Eval (eval)
import System.FilePath (takeFileName, (</>), splitExtension, replaceExtension)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, removeFile)
import qualified CodeGenLaTeX as CodeGenLaTeX
import CodeGenLaTeX (codeGenLaTeX)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad (when)
import Types (CLIOptions (..), Fragment (EvalFragment))
import Language.Haskell.Interpreter (runInterpreter, loadModules, setTopLevelModules, interpret, as)
import qualified Data.Map as Map
import IR (IRExpr)

compile :: CLIOptions -> IO ()
compile options = do
    resFile <- processFile $ filePath options
    text <- readFile resFile

    putStrLn $ "Compiling file: " ++ resFile

    let (fileName, _) = splitExtension $ takeFileName resFile
    (pathHaskell, pathLaTeX, dir) <- outPaths fileName $ outDir options

    let modName = moduleName options

    let parsed = parse text
    let elaborated = elab parsed
    let (lib, evalFrags) = codeGenHaskell elaborated modName

    writeFile pathHaskell lib
    
    -- haskellEvalRess <- eval pathHaskell (moduleName options) ["g(1, 2)", "3 / g(2,3)"] (snd elaborated)
    -- let latexEvalRess = map haskellToLaTeX haskellEvalRess
    resFrags <- eval pathHaskell modName evalFrags 
    let elaborated' = replaceEvalFrags elaborated resFrags

    -- mapM_ putStrLn latexEvalRess

    let tmpLatex = codeGenLaTeX elaborated'
    let latex = case wrapDoc options of
            True -> "\\documentclass{article}\n\\usepackage{amsmath, amssymb, hyperref}\n\n\\begin{document}\n\n" ++ tmpLatex ++ "\n\n\\end{document}"
            False -> tmpLatex
    
    writeFile pathLaTeX latex

    putStrLn "NBM Compiled successfully!"

    when (toPDF options) (compileToPDF pathLaTeX dir)

replaceEvalFrags :: ElabResult -> [IRExpr] -> ElabResult
replaceEvalFrags [] _ = []
replaceEvalFrags rs [] = rs
replaceEvalFrags ((EvalFragment _):rs) (e:es) = (EvalFragment e) : replaceEvalFrags rs es
replaceEvalFrags (r:rs) es = r : replaceEvalFrags rs es

processFile :: FilePath -> IO FilePath
processFile file = do
    let (_, ext) = splitExtension $ takeFileName file
    exists <- doesFileExist file

    let resFile = case exists of
            True -> file
            False -> case ext of
                "nbm" -> file
                _ -> file ++ ".nbm"

    return resFile

processOutDir :: FilePath -> IO FilePath
processOutDir out = do
    dir <- canonicalizePath out

    -- create the directory if it doesn't exist
    createDirectoryIfMissing True dir

    return dir

outPaths :: FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
outPaths name out = do
    dir <- processOutDir out
    return $ (dir </> (name ++ ".hs"), dir </> (name ++ ".tex"), dir)

-- eval :: FilePath -> String -> [String] -> Scope -> IO [String]
-- eval file modName exprs scope = do
--     let parsed = map runExprParser exprs
--     let elaborated = map (`elabTopLevelExpr` scope) parsed
--     let code = map (`CodeGenHaskell.codeGenExpr` modName) elaborated 
    
--     result <- runInterpreter $ do
--         loadModules [file]
--         setTopLevelModules [modName]
--         let showExprs = map ("show $ " ++) code
--         mapM (`interpret` (as :: String)) showExprs 

--     case result of
--         Right val -> return val
--         Left err -> error $ show err

-- haskellToLaTeX :: String -> String
-- haskellToLaTeX haskell = CodeGenLaTeX.codeGenExpr elaborated
--     where   nbm = haskellToNBM haskell
--             parsed = runExprParser nbm
--             elaborated = elabTopLevelExpr parsed Map.empty  -- there should be no identifiers left

-- haskellToNBM :: String -> String
-- haskellToNBM [] = []
-- haskellToNBM ('%':cs) = '/' : haskellToNBM cs
-- haskellToNBM (c:cs) = c : haskellToNBM cs

compileToPDF :: FilePath -> FilePath -> IO ()
compileToPDF texFile dir = do
    let options = ["-interaction=nonstopmode", "-output-directory=" ++ dir, texFile]
    
    -- we run pdflatex twice to resolve references
    _ <- readProcessWithExitCode "pdflatex" options ""
    (exitCode, stdout, _) <- readProcessWithExitCode "pdflatex" options ""

    case exitCode of
        ExitSuccess -> do
            putStrLn "PDF generation successful!"
            cleanUpPDFArtifacts texFile
        ExitFailure _ -> do
            putStrLn "LaTeX compilation with pdflatex failed"
            mapM_ putStrLn (lines stdout)

cleanUpPDFArtifacts :: FilePath -> IO ()
cleanUpPDFArtifacts texFile = do
    let replace = replaceExtension texFile
    let (auxFile, logFile, outFile) = (replace "aux", replace "log", replace "out")

    removeIfExists auxFile
    removeIfExists logFile
    removeIfExists outFile

removeIfExists :: FilePath -> IO ()
removeIfExists file = do
    exists <- doesFileExist file
    when exists (removeFile file)