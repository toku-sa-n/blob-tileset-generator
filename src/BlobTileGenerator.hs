module BlobTileGenerator
    ( isCorrectSize
    , isImage1x5Size
    , isWidthMoreThanOrEqualTo4
    , isWidthEven
    ) where

import           Codec.Picture (Image (Image))

isCorrectSize :: Image a -> Bool
isCorrectSize img = isImage1x5Size img && isWidthEven img

isImage1x5Size :: Image a -> Bool
isImage1x5Size (Image w h _) = w * 5 == h

isWidthMoreThanOrEqualTo4 :: Image a -> Bool
isWidthMoreThanOrEqualTo4 (Image w _ _) = w >= 4

isWidthEven :: Image a -> Bool
isWidthEven (Image w _ _) = even w
