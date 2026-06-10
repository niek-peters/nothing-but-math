module TestUtils (testGolden) where
    
import System.Directory (canonicalizePath, listDirectory)
import System.FilePath (takeBaseName, (</>), addExtension)
import Test.Hspec.Golden hiding (golden)
import Test.Hspec (Spec, it, runIO)

testGolden :: String -> String -> String -> (FilePath -> String -> IO String) -> Spec
testGolden inDir outDir msg f = do
    specs <- runIO $ shouldMapToGolden inDir outDir msg f
    sequence_ specs

shouldMapToGolden :: String -> String -> String -> (FilePath -> String -> IO String) -> IO [Spec]
shouldMapToGolden inDir outDir msg f = do
    inputDir <- canonicalizePath inDir
    outputDir <- canonicalizePath outDir
    inputFiles <- map (\file -> inputDir </> file) <$> listDirectory inputDir

    mapM (\file -> shouldBeGolden file outputDir msg $ f file) inputFiles

shouldBeGolden :: FilePath -> FilePath -> String -> (String -> IO String) -> IO Spec
shouldBeGolden file outDir msg f = do
    let name = takeBaseName file
    let withExt = addExtension $ outDir </> name

    res <- f =<< readFile (withExt "hs")
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