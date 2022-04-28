{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module BlobTileGenerator
    ( Tile1x5
    , isCorrectSize
    , splitImage
    , fromPartsUnchecked
    ) where

import           Codec.Picture       (Image (Image, imageWidth),
                                      Pixel (PixelBaseComponent))
import           Codec.Picture.Extra (crop)
import           Control.Lens        (makeLenses, (^.))
import           Foreign             (Storable)

data Tile1x5 a =
    Tile1x5
        { _noConnection         :: Image a
        , _verticalConnection   :: Image a
        , _horizontalConnection :: Image a
        , _innerCorner          :: Image a
        , _allConnection        :: Image a
        }

makeLenses ''Tile1x5

instance (Eq (PixelBaseComponent a), Storable (PixelBaseComponent a)) =>
         Eq (Tile1x5 a) where
    t1 == t2 =
        t1 ^. noConnection == t2 ^. noConnection &&
        t1 ^. verticalConnection == t2 ^. verticalConnection &&
        t1 ^. horizontalConnection == t2 ^. horizontalConnection &&
        t1 ^. innerCorner == t2 ^. innerCorner &&
        t1 ^. allConnection == t2 ^. allConnection

splitImage :: Pixel a => Image a -> Maybe (Tile1x5 a)
splitImage img
    | isCorrectSize img =
        Just $
        Tile1x5 (head parts) (parts !! 1) (parts !! 2) (parts !! 3) (parts !! 4)
    | otherwise = Nothing
  where
    parts = fmap (\y -> crop 0 y w w img) topYCoord
    topYCoord = take 5 [0,w ..]
    w = imageWidth img

fromPartsUnchecked ::
       Image a -> Image a -> Image a -> Image a -> Image a -> Tile1x5 a
fromPartsUnchecked = Tile1x5

isCorrectSize :: Image a -> Bool
isCorrectSize img = isImage1x5Size img && isWidthEven img

isImage1x5Size :: Image a -> Bool
isImage1x5Size (Image w h _) = w * 5 == h

isWidthEven :: Image a -> Bool
isWidthEven (Image w _ _) = even w
