{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module BlobTileGenerator
    ( Tile1x5
    , isCorrectSize
    , splitImage
    , fromPartsUnchecked
    ) where

import           Codec.Picture       (Image (Image, imageHeight, imageWidth),
                                      Pixel (PixelBaseComponent))
import           Codec.Picture.Extra (crop)
import           Control.Lens        (makeLenses, (^.))
import           Foreign             (Storable)

data TileSplitIntoFourDirections a =
    TileSplitIntoFourDirections
        { _northWest :: Image a
        , _northEast :: Image a
        , _southWest :: Image a
        , _southEast :: Image a
        }

makeLenses ''TileSplitIntoFourDirections

instance (Eq (PixelBaseComponent a), Storable (PixelBaseComponent a)) =>
         Eq (TileSplitIntoFourDirections a) where
    t1 == t2 =
        t1 ^. northWest == t2 ^. northWest &&
        t1 ^. northEast == t2 ^. northEast &&
        t1 ^. southWest == t2 ^. southWest && t1 ^. southEast == t2 ^. southEast

data Tile1x5 a =
    Tile1x5
        { _noConnection         :: TileSplitIntoFourDirections a
        , _verticalConnection   :: TileSplitIntoFourDirections a
        , _horizontalConnection :: TileSplitIntoFourDirections a
        , _innerCorner          :: TileSplitIntoFourDirections a
        , _allConnection        :: TileSplitIntoFourDirections a
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
        fromPartsUnchecked
            (head parts)
            (parts !! 1)
            (parts !! 2)
            (parts !! 3)
            (parts !! 4)
    | otherwise = Nothing
  where
    parts = fmap (\y -> crop 0 y w w img) topYCoord
    topYCoord = take 5 [0,w ..]
    w = imageWidth img

fromPartsUnchecked ::
       Pixel a
    => Image a
    -> Image a
    -> Image a
    -> Image a
    -> Image a
    -> Tile1x5 a
fromPartsUnchecked n v h i a = Tile1x5 n' v' h' i' a'
  where
    n' = tileSplitIntoFourDirections n
    v' = tileSplitIntoFourDirections v
    h' = tileSplitIntoFourDirections h
    i' = tileSplitIntoFourDirections i
    a' = tileSplitIntoFourDirections a

tileSplitIntoFourDirections ::
       Pixel a => Image a -> TileSplitIntoFourDirections a
tileSplitIntoFourDirections img = TileSplitIntoFourDirections nw ne sw se
  where
    nw = cropToQuarter 0 0
    ne = cropToQuarter 0 wHalf
    sw = cropToQuarter hHalf 0
    se = cropToQuarter hHalf wHalf
    cropToQuarter x y = crop x y wHalf hHalf img
    wHalf = w `div` 2
    hHalf = h `div` 2
    w = imageWidth img
    h = imageHeight img

isCorrectSize :: Image a -> Bool
isCorrectSize img = isImage1x5Size img && isWidthEven img

isImage1x5Size :: Image a -> Bool
isImage1x5Size (Image w h _) = w * 5 == h

isWidthEven :: Image a -> Bool
isWidthEven (Image w _ _) = even w
