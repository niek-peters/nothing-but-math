module TestUtils (testGolden, shouldThrowInPhase) where
    
import System.Directory (canonicalizePath, listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeBaseName, (</>), addExtension)
import Test.Hspec.Golden hiding (golden)
import Test.Hspec (Spec, it, runIO, Expectation, anyErrorCall, shouldThrow)
import Control.Monad (filterM)

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

listOnlyFiles :: FilePath -> IO [FilePath]
listOnlyFiles dir = listDirectory dir >>= filterM (doesFileExist . (dir </>))

shouldThrowInPhase :: (Show b) => String -> (String -> IO a) -> (a -> IO b) -> Expectation
shouldThrowInPhase name prep final = do
    file <- canonicalizePath name
    src <- readFile file
    tmp <- prep src -- it should not throw an error in previous compiler phases

    -- then it should throw in the final phase
    shouldThrow (final tmp >>= print) anyErrorCall  -- print forces evaluation (but nothing will be printed if there is an error as we expect)