{-# LANGUAGE TupleSections #-}

module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator   (TileType (CornerInside, CornerOutside, Horizontal, NoBorder, Vertical),
                                      fromPartsUnchecked, fromTypesUnchecked,
                                      indexToTile, indexToTileTypes,
                                      isCorrectSize, splitImage)
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
    testIndexToTile

testSplitImage :: Spec
testSplitImage =
    describe "tile1x5" $ do
        it
            "returns a Just value including the Tile1x5 type value containing images separated into 5. if the image has a correct size." $
            splitImage correctTileFile == Just expected
        it "returns a Nothing if the given tile has an incorrect size." $
            and $ fmap (isNothing . splitImage) incorrectSizeImages
  where
    correctTileFile = below [t1, t2, t3, t4, t5]
    expected = fromPartsUnchecked t1 t2 t3 t4 t5
    (t1, t2, t3, t4, t5) =
        case fmap blueToTile [0 .. 4] of
            [a, b, c, d, e] -> (a, b, c, d, e)
            _               -> error "Unexpected list length"
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
            success 112 Horizontal CornerOutside NoBorder Vertical
        it "returns a Nothing value if the index is invalid." $
            indexToTileTypes 334 `shouldBe` Nothing
  where
    success index nw ne sw se =
        indexToTileTypes index `shouldBe` Just (fromTypesUnchecked nw ne sw se)

testIndexToTile :: Spec
testIndexToTile =
    describe "indexToTile" $
    it "returns a Just value containing a tile image generated by a Tile1x5 value" $
    (indexToTile tile1x5Image 85 == Just expected) &&
    (indexToTile tile1x5Image 255 == Just expected2) &&
    (indexToTile tile1x5Image 17 == Just expected3) &&
    (indexToTile tile1x5Image 5 == Just expected4)
  where
    expected = generateImage (\_ _ -> PixelRGB8 0 0 3) 2 2
    expected2 = generateImage (\_ _ -> PixelRGB8 0 0 4) 2 2
    expected3 = generateImage (\_ _ -> PixelRGB8 0 0 1) 2 2
    expected4 =
        generateImage
            (\x y ->
                 PixelRGB8
                     0
                     0
                     (case (x, y) of
                          (0, 0) -> 1
                          (1, 0) -> 3
                          (0, 1) -> 0
                          (1, 1) -> 2
                          _      -> undefined))
            2
            2
    tile1x5Image = fromPartsUnchecked t1 t2 t3 t4 t5
    (t1, t2, t3, t4, t5) =
        case fmap blueToTile [0 .. 4] of
            [a, b, c, d, e] -> (a, b, c, d, e)
            _               -> error "Unexpected list length"
    blueToTile b = generateImage (\_ _ -> PixelRGB8 0 0 b) 2 2

incorrectSizeImages :: [Image PixelRGB8]
incorrectSizeImages = [oddWidth, incorrectRatio]
  where
    oddWidth = generateBlackImage 1 5
    incorrectRatio = generateBlackImage 2 9

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
