{-# LANGUAGE LambdaCase #-}

module Main
    ( main
    ) where

import           BlobTileGenerator          (generateBlobTile)
import           Codec.Picture              (Image, Pixel, PixelRGBA8,
                                             convertRGBA8, readImage, writePng)
import           Control.Monad.Trans.Except (ExceptT (ExceptT), except,
                                             runExceptT)
import           Data.Either.Combinators    (maybeToRight)
import           Options.Applicative        (Parser, ParserInfo, execParser,
                                             fullDesc, header, help, helper,
                                             info, long, metavar, progDesc,
                                             short, showDefault, strArgument,
                                             strOption, value, (<**>))
import           System.Exit                (ExitCode (ExitFailure), exitWith)
import           System.IO                  (hPutStrLn, stderr)

data Argument =
    Argument
        { output :: FilePath
        , input  :: FilePath
        }

main :: IO ()
main =
    execParser parserInfo >>= runExceptT . runOrErr >>= \case
        Right () -> return ()
        Left e   -> exitWithErrMsg e

parserInfo :: ParserInfo Argument
parserInfo =
    info
        (argumentParser <**> helper)
        (fullDesc <>
         progDesc
             "Convert a 1x5 tile image specified as FILE and generate the blob tile image to TARGET" <>
         header
             "blob-tile-generator - Convert a 1x5 tile imager to the blob tile one.")

argumentParser :: Parser Argument
argumentParser =
    Argument <$>
    strOption
        (long "output" <>
         short 'o' <>
         metavar "TARGET" <>
         showDefault <> value "blob_tile.png" <> help "Output to <TARGET>") <*>
    strArgument (metavar "FILE")

runOrErr :: Argument -> ExceptT String IO ()
runOrErr arg =
    readImageOrErr (input arg) >>= generateBlobTileOrErr >>=
    ExceptT . fmap Right . writePng (output arg)

readImageOrErr :: FilePath -> ExceptT String IO (Image PixelRGBA8)
readImageOrErr = ExceptT . fmap (fmap convertRGBA8) . readImage

generateBlobTileOrErr :: Pixel a => Image a -> ExceptT String IO (Image a)
generateBlobTileOrErr = except . maybeToRight msg . generateBlobTile
  where
    msg = "Failed to convert the tile image. Please check the image's size."

exitWithErrMsg :: String -> IO ()
exitWithErrMsg msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)
