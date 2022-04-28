module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator   (fromPartsUnchecked, isCorrectSize,
                                      splitImage)
import           Codec.Picture       (Image, PixelRGB8 (PixelRGB8),
                                      generateImage)
import           Codec.Picture.Extra (below)
import           Test.Hspec          (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIsCorrectSize
    testSplitImage

testSplitImage :: Spec
testSplitImage =
    describe "tile1x5" $
    it "returns a Just value including the Tile1x5 type value containing images separated into 5. if the image has a correct size." $
    splitImage correctTileFile == Just expected
  where
    correctTileFile = below tiles
    expected =
        fromPartsUnchecked
            (head tiles)
            (tiles !! 1)
            (tiles !! 2)
            (tiles !! 3)
            (tiles !! 4)
    tiles = fmap blueToTile [0 .. 4]
    blueToTile b = generateImage (\_ _ -> PixelRGB8 0 0 b) 2 2

testIsCorrectSize :: Spec
testIsCorrectSize =
    describe "isCorrectSize" $
    it "returns True if the given has a width-to-height ratio of 1:5, and an even width." $
    mapM_ checkFunc patterns
  where
    patterns = [(correctSize, True), (oddWidth, False), (incorrectRatio, False)]
    checkFunc (img, expected) = isCorrectSize img `shouldBe` expected
    correctSize = generateBlackImage 2 10
    oddWidth = generateBlackImage 1 5
    incorrectRatio = generateBlackImage 2 9

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
