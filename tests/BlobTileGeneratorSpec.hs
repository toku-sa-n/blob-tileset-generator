module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator (isImage1x5Size, isWidthMoreThanOrEqualTo4)
import           Codec.Picture     (Image, PixelRGB8 (PixelRGB8), generateImage)
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "isImage1x5Size" $ do
        it "returns True if the given image has a width-to-height ratio of 1:5." $
            isImage1x5Size correctRatio `shouldBe` True
        it "returns False otherwise" $
            isImage1x5Size incorrectRatio `shouldBe` False
    describe "isWidthMoreThanOrEqualTo4" $
        it
            "returns True if the width of the given image is more than or equal to 4." $
        isWidthMoreThanOrEqualTo4 enoughWidth `shouldBe` True
  where
    enoughWidth = generateBlackImage 5 1
    correctRatio = generateBlackImage 1 5
    incorrectRatio = generateBlackImage 2 5

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
