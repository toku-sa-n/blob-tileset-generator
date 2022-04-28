module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator (isImage1x5Size, isWidthMoreThanOrEqualTo4)
import           Codec.Picture     (Image, PixelRGB8 (PixelRGB8), generateImage)
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIsImage1x5Size
    testIsWidthMoreThanOrEqualTo4

testIsImage1x5Size :: Spec
testIsImage1x5Size =
    describe "isImage1x5Size" $ do
        it "returns True if the given image has a width-to-height ratio of 1:5." $
            isImage1x5Size correctRatio `shouldBe` True
        it "returns False otherwise" $
            isImage1x5Size incorrectRatio `shouldBe` False
  where
    correctRatio = generateBlackImage 1 5
    incorrectRatio = generateBlackImage 2 5

testIsWidthMoreThanOrEqualTo4 :: Spec
testIsWidthMoreThanOrEqualTo4 =
    describe "isWidthMoreThanOrEqualTo4" $ do
        it
            "returns True if the width of the given image is more than or equal to 4." $
            isWidthMoreThanOrEqualTo4 enoughWidth `shouldBe` True
        it "returns False otherwise" $
            isWidthMoreThanOrEqualTo4 notEnoughWidth `shouldBe` False
  where
    enoughWidth = generateBlackImage 5 1
    notEnoughWidth = generateBlackImage 3 8

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
