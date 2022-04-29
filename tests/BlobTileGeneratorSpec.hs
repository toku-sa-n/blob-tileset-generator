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
            indexToTileTypes 85 `shouldBe`
                Just
                    (fromTypesUnchecked
                         CornerInside
                         CornerInside
                         CornerInside
                         CornerInside)
            indexToTileTypes 255 `shouldBe`
                Just (fromTypesUnchecked NoBorder NoBorder NoBorder NoBorder)
            indexToTileTypes 17 `shouldBe`
                Just (fromTypesUnchecked Vertical Vertical Vertical Vertical)
            indexToTileTypes 5 `shouldBe`
                Just
                    (fromTypesUnchecked
                         Vertical
                         CornerInside
                         CornerOutside
                         Horizontal)
            indexToTileTypes 68 `shouldBe`
                Just
                    (fromTypesUnchecked
                         Horizontal
                         Horizontal
                         Horizontal
                         Horizontal)
            indexToTileTypes 29 `shouldBe`
                Just
                    (fromTypesUnchecked Vertical CornerInside Vertical NoBorder)
        it "returns a Nothing value if the index is invalid." $
            indexToTileTypes 334 `shouldBe` Nothing

incorrectSizeImages :: [Image PixelRGB8]
incorrectSizeImages = [oddWidth, incorrectRatio]
  where
    oddWidth = generateBlackImage 1 5
    incorrectRatio = generateBlackImage 2 9

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
