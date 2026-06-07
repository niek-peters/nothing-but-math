module TestUtils (shouldBe', fileProcessedShouldBe) where
    
import Test.Hspec (shouldBe, Expectation)
import System.Directory (canonicalizePath)

shouldBe' :: (Show a, Eq a) => IO a -> a -> Expectation
shouldBe' a b = a >>= (`shouldBe` b)

fileProcessedShouldBe :: (Show a, Eq a) => FilePath -> (String -> a) -> a -> Expectation
fileProcessedShouldBe file f val = (f <$> (readFile =<< canonicalizePath file)) `shouldBe'` val