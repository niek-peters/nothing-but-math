module TestUtils (shouldBeGolden, shouldBeGolden') where
    
import System.Directory (canonicalizePath)
import System.FilePath (splitExtension, addExtension)
import Test.Hspec.Golden

shouldBeGolden :: String -> (String -> String) -> IO (Golden String)
shouldBeGolden file f = do
    inputFile <- canonicalizePath file
    res <- f <$> readFile inputFile
    
    return $ makeGolden inputFile res

shouldBeGolden' :: String -> (String -> IO String) -> IO (Golden String)
shouldBeGolden' file f = do
    inputFile <- canonicalizePath file
    res <- f =<< readFile inputFile
    return $ makeGolden inputFile res

makeGolden :: FilePath -> String -> Golden String
makeGolden file res = Golden {
        output = res,
        encodePretty = id,
        writeToFile = writeFile,
        readFromFile = readFile,
        goldenFile = withExt "expected",
        failFirstTime = False,
        actualFile = Nothing
    }
    where   withExt = addExtension $ fst $ splitExtension file