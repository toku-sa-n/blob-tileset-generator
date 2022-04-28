module BlobTileGenerator
    ( isImage1x5Size
    ) where

import           Codec.Picture (Image (Image))

isImage1x5Size :: Image a -> Bool
isImage1x5Size (Image w h _) = w * 5 == h
