# Blob tileset generator

A program to generate a [blob tileset](http://www.cr31.co.uk/stagecast/wang/blob.html) from a 1x5 tileset image. You can create a map with the generated blob tileset using [Tiled](https://www.mapeditor.org/).

## Usage

```
cabal run blob-tileset-generator.cabal -- <1x5 tileset image path>
```

The 1x5 tileset image must satisfy that
- `width * 5 == height`.
- width is even.

## Examples

|Input|Output|
|-----|------|
|![input](examples/example_input.png)|![output](examples/example_output.png)|
|![input](examples/wall_1x5.png)|![output](examples/wall_blob.png)|

## Licenses

GPL version 3.0. See [LICENSE](LICENSE)
