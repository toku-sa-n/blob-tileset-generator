{-# LANGUAGE TupleSections #-}

module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator   (TileType (CornerInside, CornerOutside, Horizontal, NoBorder, Vertical),
                                      fromPartsUnchecked, fromTypesUnchecked,
                                      indexToTileTypes, isCorrectSize,
                                      splitImage)
import           Codec.Picture       (Image, PixelRGB8 (PixelRGB8),
                                      generateImage)
import           Codec.Picture.Extra (below)
import           Data.Maybe          (isNothing)
import           Test.Hspec          (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIsCorrectSize
    testSplitImage
    testIndexToTileTypes

testSplitImage :: Spec
testSplitImage =
    describe "tile1x5" $ do
        it
            "returns a Just value including the Tile1x5 type value containing images separated into 5. if the image has a correct size." $
            splitImage correctTileFile == Just expected
        it "returns a Nothing if the given tile has an incorrect size." $
            and $ fmap (isNothing . splitImage) incorrectSizeImages
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
    patterns = [(correctSize, True)] <> fmap (, False) incorrectSizeImages
    checkFunc (img, expected) = isCorrectSize img `shouldBe` expected
    correctSize = generateBlackImage 2 10

testIndexToTileTypes :: Spec
testIndexToTileTypes =
    describe "indexToTileTypes" $ do
        it "returns a Just value containing tile types of each corner" $ do
            success 85 CornerInside CornerInside CornerInside CornerInside
            success 255 NoBorder NoBorder NoBorder NoBorder
            success 17 Vertical Vertical Vertical Vertical
            success 5 Vertical CornerInside CornerOutside Horizontal
            success 68 Horizontal Horizontal Horizontal Horizontal
            success 29 Vertical CornerInside Vertical NoBorder
        it "returns a Nothing value if the index is invalid." $
            indexToTileTypes 334 `shouldBe` Nothing
  where
    success index nw ne sw se =
        indexToTileTypes index `shouldBe` Just (fromTypesUnchecked nw ne sw se)

incorrectSizeImages :: [Image PixelRGB8]
incorrectSizeImages = [oddWidth, incorrectRatio]
  where
    oddWidth = generateBlackImage 1 5
    incorrectRatio = generateBlackImage 2 9

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
