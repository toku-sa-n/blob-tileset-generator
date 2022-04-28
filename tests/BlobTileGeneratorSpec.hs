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
    correctTileFile =
        below [redTile, blueTile, greenTile, yellowTile, blackTile]
    expected =
        fromPartsUnchecked redTile blueTile greenTile yellowTile blackTile
    redTile = pixelToTile 255 0 0
    blueTile = pixelToTile 0 0 255
    greenTile = pixelToTile 0 255 0
    yellowTile = pixelToTile 255 255 0
    blackTile = pixelToTile 0 0 0
    pixelToTile r g b = generateImage (\_ _ -> PixelRGB8 r g b) 2 2

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
