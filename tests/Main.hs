module Main
    ( main
    ) where

import qualified BlobTilesetGeneratorSpec
import           Test.Hspec               (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "BlobTilesetGenerator" BlobTilesetGeneratorSpec.spec
