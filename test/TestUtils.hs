-- | Shared helpers for golden tests and failure assertions.
--
-- The functions here keep the phase specs concise by handling golden testing of whole directories and compilation phase-specific error expectations.
module TestUtils (testGolden, shouldThrowInPhase, apply2) where

import System.Directory (canonicalizePath, listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeBaseName, (</>), addExtension, splitExtension)
import Test.Hspec.Golden hiding (golden)
import Test.Hspec (Spec, it, runIO, Expectation, anyErrorCall, shouldThrow)
import Control.Monad (filterM)
import Control.Exception (evaluate)

-- | Build a golden test group from every file in an input directory.
testGolden :: String -> String -> String -> (FilePath -> String -> IO String) -> Spec
testGolden inDir outDir msg f = do
    specs <- runIO $ shouldMapToGolden inDir outDir msg f
    sequence_ specs

shouldMapToGolden :: String -> String -> String -> (FilePath -> String -> IO String) -> IO [Spec]
shouldMapToGolden inDir outDir msg f = do
    inputDir <- canonicalizePath inDir
    outputDir <- canonicalizePath outDir
    
    -- create the output directory if it doesn't exist
    createDirectoryIfMissing True outputDir

    inputFiles <- map (\file -> inputDir </> file) <$> listOnlyFiles inputDir

    mapM (\file -> shouldBeGolden file outputDir msg f) inputFiles

shouldBeGolden :: FilePath -> FilePath -> String -> (FilePath -> String -> IO String) -> IO Spec
shouldBeGolden file outDir msg f = do
    let name = takeBaseName file
    let withExt = addExtension $ outDir </> name

    res <- f (withExt "generated.hs") =<< readFile file
    let golden = makeGolden (withExt "expected") res
    return $ it (msg ++ " " ++ name) golden

makeGolden :: FilePath -> String -> Golden String
makeGolden file res  = Golden {
        output = res,
        encodePretty = id,
        writeToFile = writeFile,
        readFromFile = readFile,
        goldenFile = file,
        failFirstTime = False,
        actualFile = Nothing
    }

-- | List only regular files in a directory.
listOnlyFiles :: FilePath -> IO [FilePath]
listOnlyFiles dir = listDirectory dir >>= filterM (doesFileExist . (dir </>))

-- | Assert that the final compiler phase throws an error for the given source file.
shouldThrowInPhase :: (Show b) => String -> (FilePath -> String -> IO a) -> (FilePath -> a -> IO b) -> Expectation
shouldThrowInPhase name prep final = do
    file <- canonicalizePath name
    let genFile = addExtension (fst $ splitExtension file) "generated.hs"
    src <- readFile file
    _ <- evaluate (length src)   -- force full evaluation of src (so the file is closed)
    tmp <- prep genFile src -- it should not throw an error in previous compiler phases

    -- then it should throw in the final phase
    shouldThrow (final genFile tmp >>= print) anyErrorCall  -- print forces evaluation (but nothing will be printed if there is an error as we expect)

-- | Apply a function over the result of a two-argument functor-producing function.
-- Useful for use with the `testGolden` function.
apply2 :: Functor f => (c -> d) -> (a -> b -> f c) -> a -> b -> f d
apply2 f g x y = f <$> g x y