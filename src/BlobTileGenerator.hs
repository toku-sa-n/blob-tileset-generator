{-# LANGUAGE UndecidableInstances #-}

module BlobTileGenerator
    ( generateBlobTile
    ) where

import           Codec.Picture       (Image (Image, imageWidth),
                                      Pixel (PixelBaseComponent), PixelRGBA8)
import           Codec.Picture.Extra (below, beside, crop)
import           Data.Bits           (Bits (bit, (.&.)))
import           Data.Maybe          (fromMaybe)
import           Foreign             (Storable)

data TileType
    = Horizontal
    | Vertical
    | CornerOutside
    | CornerInside
    | NoBorder
    deriving (Show, Eq)

data Corners a =
    Corners
        { northWest :: a
        , northEast :: a
        , southWest :: a
        , southEast :: a
        }
    deriving (Show, Eq)

instance Functor Corners where
    fmap f c =
        Corners
            (f (northWest c))
            (f (northEast c))
            (f (southWest c))
            (f (southEast c))

type TileTypesOfCorners = Corners TileType

type TileSplitIntoFourDirections a = Corners (Image a)

data Tile1x5 a =
    Tile1x5
        { noConnection         :: TileSplitIntoFourDirections a
        , verticalConnection   :: TileSplitIntoFourDirections a
        , horizontalConnection :: TileSplitIntoFourDirections a
        , innerCorner          :: TileSplitIntoFourDirections a
        , allConnection        :: TileSplitIntoFourDirections a
        }

instance (Eq (PixelBaseComponent a), Storable (PixelBaseComponent a)) =>
         Eq (Tile1x5 a) where
    t1 == t2 =
        and $
        fmap
            (\x -> x t1 == x t2)
            [ noConnection
            , verticalConnection
            , horizontalConnection
            , innerCorner
            , allConnection
            ]

generateBlobTile :: Image PixelRGBA8 -> Maybe (Image PixelRGBA8)
generateBlobTile img =
    fmap
        ((below . fmap beside) .
         (\t1x5 -> fmap (fmap (unwrap . indexToTile t1x5)) minimumPacking))
        (splitImage img)
  where
    unwrap = fromMaybe (error "Failed to convert an index to a tile.")

indexToTile :: Pixel a => Tile1x5 a -> Int -> Maybe (Image a)
indexToTile t1x5 =
    fmap (concatParts . fmap (`typeToImg` t1x5)) . indexToTileTypes

concatParts :: Pixel a => Corners (TileSplitIntoFourDirections a) -> Image a
concatParts (Corners nw ne sw se) = below [upper, lower]
  where
    upper = beside [northWest nw, northEast ne]
    lower = beside [southWest sw, southEast se]

typeToImg :: TileType -> Tile1x5 a -> TileSplitIntoFourDirections a
typeToImg NoBorder      = allConnection
typeToImg Vertical      = verticalConnection
typeToImg Horizontal    = horizontalConnection
typeToImg CornerOutside = noConnection
typeToImg CornerInside  = innerCorner

indexToTileTypes :: Int -> Maybe TileTypesOfCorners
indexToTileTypes index
    | index `elem` validIndexes =
        case fmap baseIndexType baseIndexes of
            [a, b, c, d] ->
                Just $
                Corners
                    (convertHorizontalVertical d)
                    a
                    c
                    (convertHorizontalVertical b)
            e -> error $ "Unexpected length of list: " <> show e
    | otherwise = Nothing
  where
    baseIndexType base =
        case ( tileExists !! base
             , tileExists !! (base + 1)
             , tileExists !! (base + 2)) of
            (False, False, False) -> CornerOutside
            (True, False, False)  -> Vertical
            (False, False, True)  -> Horizontal
            (True, False, True)   -> CornerInside
            (True, True, True)    -> NoBorder
            _                     -> error $ "Unexpected index: " <> show index
    convertHorizontalVertical Horizontal = Vertical
    convertHorizontalVertical Vertical   = Horizontal
    convertHorizontalVertical x          = x
    tileExists = fmap ((/= 0) . (index .&.) . bit) ([0 .. 7] <> [0])
    baseIndexes = [0, 2, 4, 6]

validIndexes :: [Int]
validIndexes =
    255 :
    concatMap
        (\x -> fmap ((`mod` 255) . (* x)) [1, 4, 16, 64])
        [0, 1, 5, 7, 17, 21, 23, 29, 31, 85, 87, 95, 119, 127]

splitImage :: Pixel a => Image a -> Maybe (Tile1x5 a)
splitImage img
    | isCorrectSize img = Just $ fromPartsUnchecked p1 p2 p3 p4 p5
    | otherwise = Nothing
  where
    (p1, p2, p3, p4, p5) =
        case fmap (\y -> crop 0 y w w img) topYCoord of
            [a, b, c, d, e] -> (a, b, c, d, e)
            _               -> error "Unexpected length of list."
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
    (n', v', h', i', a') =
        case fmap tileSplitIntoFourDirections [n, v, h, i, a] of
            [p, q, r, s, t] -> (p, q, r, s, t)
            _               -> error "Unexpected length of list."

tileSplitIntoFourDirections ::
       Pixel a => Image a -> TileSplitIntoFourDirections a
tileSplitIntoFourDirections img = Corners nw ne sw se
  where
    nw = cropToQuarter 0 0
    ne = cropToQuarter wHalf 0
    sw = cropToQuarter 0 wHalf
    se = cropToQuarter wHalf wHalf
    cropToQuarter x y = crop x y wHalf wHalf img
    wHalf = w `div` 2
    w = imageWidth img

isCorrectSize :: Image a -> Bool
isCorrectSize img = isImage1x5Size img && isWidthEven img

isImage1x5Size :: Image a -> Bool
isImage1x5Size (Image w h _) = w * 5 == h

isWidthEven :: Image a -> Bool
isWidthEven (Image w _ _) = even w

minimumPacking :: [[Int]]
minimumPacking =
    [ [20, 68, 92, 112, 28, 124, 116, 80]
    , [21, 84, 87, 221, 127, 255, 241, 17]
    , [29, 117, 85, 95, 247, 215, 209, 1]
    , [23, 213, 81, 31, 253, 125, 113, 16]
    , [5, 69, 93, 119, 223, 255, 245, 65]
    , [0, 4, 71, 193, 7, 199, 197, 64]
    ]
