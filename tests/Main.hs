module Main
    ( main
    ) where

import qualified BlobTileGeneratorSpec
import           Test.Hspec            (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "BlobTileGenerator" BlobTileGeneratorSpec.spec
