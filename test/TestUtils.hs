module TestUtils (shouldBe', fileProcessedShouldBe, shouldBeGolden) where
    
import Test.Hspec
import System.Directory (canonicalizePath)
import System.FilePath (splitExtension, addExtension)
import Test.Hspec.Golden
import Text.Show.Pretty (pPrint, ppShow)

shouldBe' :: (Show a, Eq a) => IO a -> a -> Expectation
shouldBe' a b = a >>= (`shouldBe` b)

fileProcessedShouldBe :: (Show a, Eq a) => FilePath -> (String -> a) -> a -> Expectation
fileProcessedShouldBe file f val = (f <$> (readFile =<< canonicalizePath file)) `shouldBe'` val

shouldBeGolden :: String -> (String -> String) -> IO (Golden String)
shouldBeGolden file f = do
    inputFile <- canonicalizePath file
    res <- f <$> readFile inputFile
    let withExt = addExtension $ fst $ splitExtension inputFile

    return Golden {
        output = res,
        encodePretty = id,
        writeToFile = writeFile,
        readFromFile = readFile,
        goldenFile = withExt "expected",
        failFirstTime = False,
        actualFile = Nothing
    }