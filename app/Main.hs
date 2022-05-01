{-# LANGUAGE LambdaCase #-}

module Main
    ( main
    ) where

import           BlobTileGenerator          (generateBlobTile)
import           Codec.Picture              (Image, Pixel, PixelRGBA8,
                                             convertRGBA8, readImage, writePng)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import           Data.Either.Combinators    (maybeToRight)
import           System.Environment         (getArgs)
import           System.Exit                (ExitCode (ExitFailure), exitWith)
import           System.IO                  (hPutStrLn, stderr)

main :: IO ()
main =
    runExceptT runOrErr >>= \case
        Right () -> return ()
        Left e   -> exitWithErrMsg e

runOrErr :: ExceptT String IO ()
runOrErr =
    getArgsOrErr >>= readImageOrErr . head >>= generateBlobTileOrErr >>=
    ExceptT . fmap Right . writePng "blob_tile.png"

getArgsOrErr :: ExceptT String IO [String]
getArgsOrErr = do
    args <- ExceptT $ fmap Right getArgs
    if null args
        then ExceptT $ return $ Left "Specify a 1x5 tile image file path."
        else ExceptT $ return $ Right args

readImageOrErr :: FilePath -> ExceptT String IO (Image PixelRGBA8)
readImageOrErr = ExceptT . fmap (fmap convertRGBA8) . readImage

generateBlobTileOrErr :: Pixel a => Image a -> ExceptT String IO (Image a)
generateBlobTileOrErr = ExceptT . return . maybeToRight msg . generateBlobTile
  where
    msg = "Failed to convert the tile image. Please check the image's size."

exitWithErrMsg :: String -> IO ()
exitWithErrMsg msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)
