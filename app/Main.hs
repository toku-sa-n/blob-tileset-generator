module Main
    ( main
    ) where

import           BlobTileGenerator  (generateBlobTile)
import           Codec.Picture      (convertRGBA8, readImage, writePng)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (ExitFailure), exitWith)
import           System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then do
            hPutStrLn stderr "Specify a 1x5 tile image file."
            exitWith (ExitFailure 1)
        else do
            img <- readImage $ head args
            case img of
                Right x ->
                    case generateBlobTile $ convertRGBA8 x of
                        Just y -> writePng "blob_tile.png" y
                        Nothing -> do
                            hPutStrLn
                                stderr
                                "Failed to convert the tile image. Please check the image's size."
                            exitWith (ExitFailure 1)
                Left e -> do
                    hPutStrLn stderr $ "Failed to load an image: " <> e
                    exitWith (ExitFailure 1)
