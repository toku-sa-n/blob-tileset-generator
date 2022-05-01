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
        then exit "Specify a 1x5 tile image file."
        else do
            img <- readImage $ head args
            case img of
                Right x ->
                    case generateBlobTile $ convertRGBA8 x of
                        Just y -> writePng "blob_tile.png" y
                        Nothing ->
                            exit
                                "Failed to convert the tile image. Please check the image's size."
                Left e -> exit $ "Failed to load an image: " <> e

exit :: String -> IO ()
exit msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)
