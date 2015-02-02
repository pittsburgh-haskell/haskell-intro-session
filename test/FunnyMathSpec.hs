-- Instructions:
--
-- Load in GHCi with ":l FunnyMathSpec".
--
-- Run tests with "main".
--
-- Use ":r" to reload after any edits.

-- "*Spec" files are discovered by HSpec during Cabal test.
module FunnyMathSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

-- Our code, in src/
import FunnyMath (add, multiply)

-- | Required for automatic discovery by Cabal.
spec :: Spec
spec = do
  describe "FunnyMath" $ do
    it "adds as expected" $ do
      add 2 2 `shouldBe` 4

    prop "distributes multiplication over addition" $ \x y z ->
      multiply x (add y z) ==
      add (multiply x y) (multiply x z)

    prop "allows multiplying two numbers in either order to get the same answer" $ \x y ->
      multiply x y  == multiply y x

-- For running standalone
main :: IO ()
main = hspec spec
