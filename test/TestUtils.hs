module TestUtils (shouldBe') where
    
import Test.Hspec (shouldBe)

shouldBe' :: (Show a, Eq a) => IO a -> a -> IO ()
shouldBe' a b = a >>= (`shouldBe` b)