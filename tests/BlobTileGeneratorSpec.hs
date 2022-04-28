module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator (isImage1x5Size)
import           Codec.Picture     (Image, PixelRGB8 (PixelRGB8), generateImage)
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "isImage1x5Size" $ do
        it "returns True if the given image has a width-to-height ratio of 1:5." $
            isImage1x5Size image `shouldBe` True
        it "returns False otherwise" $ isImage1x5Size image2 `shouldBe` False
  where
    image = generateBlackImage 1 5
    image2 = generateBlackImage 2 5

generateBlackImage :: Int -> Int -> Image PixelRGB8
generateBlackImage = generateImage (\_ _ -> PixelRGB8 0 0 0)
