module BlobTileGeneratorSpec
    ( spec
    ) where

import           BlobTileGenerator (isImage1x5Size)
import           Codec.Picture     (PixelRGB8 (PixelRGB8), generateImage)
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "isImage1x5Size" $
    it "returns True if the given image has a width-to-height ratio of 1:5." $
    isImage1x5Size image `shouldBe` True
  where
    image = generateImage (\_ _ -> PixelRGB8 0 0 0) 1 5
