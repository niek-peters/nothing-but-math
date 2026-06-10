module Parsing.ParsingSpec (spec) where

import Parser (parse)
import Lexer (tokenize)

import Test.Hspec
import Test.Hspec.Golden hiding (golden)
-- import TestUtils (shouldBeGolden)
import Text.Show.Pretty (ppShow)
import System.FilePath (addExtension, splitExtension, splitFileName, (</>))
import System.Directory (canonicalizePath, getDirectoryContents, listDirectory)


-- spec :: Spec
-- spec = 
--     describe "Sample Program Parsing" $
--         goldenTestFiles ["test1", "test2", "test3", "test4", "test5"]
--     where   goldenTestFiles = mapM_ shouldParseToGolden

-- shouldParseToGolden :: String -> Spec
-- shouldParseToGolden file = it ("correctly parses example program " ++ file) $ shouldBeGolden ("test/Parsing/" ++ file ++ ".nbm") f 
--     where   f = ppShow . parse . tokenize

spec :: Spec
spec =
    describe "Sample Program Parsing" $ 
        testGolden "test/samples" "test/samples/results/Parsing" "correctly parses example program" (pure . ppShow . parse . tokenize)

testGolden :: String -> String -> String -> (String -> IO String) -> Spec
testGolden inDir outDir msg f = do
    specs <- runIO $ shouldMapToGolden inDir outDir msg f
    sequence_ specs

shouldMapToGolden :: String -> String -> String -> (String -> IO String) -> IO [Spec]
shouldMapToGolden inDir outDir msg f = do
    inputDir <- canonicalizePath inDir
    outputDir <- canonicalizePath outDir
    inputFiles <- map (\file -> inputDir </> file) <$> listDirectory inputDir

    -- putStrLn inputDir
    -- putStrLn outputDir
    -- mapM_ putStrLn inputFiles

    mapM (\file -> shouldBeGolden file outputDir msg f) inputFiles 
    -- mapM (\file -> do
    --     let (_, name) = splitFileName file
    --     res <- f =<< readFile file
    --     let golden = makeGolden file outputDir res
    --     it (msg ++ " " ++ name) golden
    --     ) inputFiles

shouldBeGolden :: FilePath -> FilePath -> String -> (String -> IO String) -> IO Spec
shouldBeGolden file outDir msg f = do
    -- putStrLn file
    let (_, name) = splitFileName file
    res <- f =<< readFile file
    let golden = makeGolden file outDir res
    return $ it (msg ++ " " ++ name) golden

-- makeGoldenDir :: String -> String -> (String -> IO String) -> IO [Golden String]
-- makeGoldenDir inDir outDir f = do
--     inputDir <- canonicalizePath inDir
--     outputDir <- canonicalizePath outDir
--     inputFiles  <- getDirectoryContents inputDir
--     mapM (\file -> (makeGolden file outputDir) <$> (f =<< readFile file)) inputFiles
--     -- inputFile <- canonicalizePath file
--     -- res <- f =<< readFile inputFile
--     -- return $ makeGolden inputFile outputDir res

makeGolden :: FilePath -> FilePath -> String -> Golden String
makeGolden file resDir res  = Golden {
        output = res,
        encodePretty = id,
        writeToFile = writeFile,
        readFromFile = readFile,
        goldenFile = resWithExt "expected",
        failFirstTime = False,
        actualFile = Nothing
    }
    where   resFile = resDir </> (snd $ splitFileName file)
            resWithExt = addExtension $ fst $ splitExtension resFile