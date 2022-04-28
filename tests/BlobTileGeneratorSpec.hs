module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator (isCorrectSize, isImage1x5Size, isWidthEven,
                                    isWidthMoreThanOrEqualTo4)
import           Codec.Picture     (Image, PixelRGB8 (PixelRGB8), generateImage)
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIsCorrectSize
    testIsImage1x5Size
    testIsWidthMoreThanOrEqualTo4
    testIsWidthEven

testIsCorrectSize :: Spec
testIsCorrectSize =
    describe "isCorrectSize" $
    it "returns True if the given has a width-to-height ratio of 1:5, and an even width." $
    mapM_ checkFunc patterns
  where
    patterns = [(correctSize, True), (oddWidth, False)]
    checkFunc (img, expected) = isCorrectSize img `shouldBe` expected
    correctSize = generateBlackImage 2 10
    oddWidth = generateBlackImage 1 5

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

testIsWidthEven :: Spec
testIsWidthEven =
    describe "isWidthEven" $ do
        it "returns True if the width of the given image is even." $
            isWidthEven correctWidth `shouldBe` True
        it "returns False otherwise" $ isWidthEven evenWidth `shouldBe` False
  where
    correctWidth = generateBlackImage 4 1
    evenWidth = generateBlackImage 1 1

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
