# blob-tile-generator

A program to generate a blob tile from 1x5 image.

## Usage

```
cabal run blob-tile-generator.cabal -- <1x5 tile image path>
```

The 1x5 tile image must satisfy that
- `width * 5 == height`.
- width is even.

## Examples

|Input|Output|
|-----|------|
|![input](https://github.com/toku-sa-n/blob-tile-generator/examples/example_input.png)|![output](https://github.com/toku-sa-n/blob-tile-generator/examples/example_output.png)|
|![input](https://github.com/toku-sa-n/blob-tile-generator/examples/wall_1x5.png)|![output](https://github.com/toku-sa-n/blob-tile-generator/examples/wall_blob.png)|

## Licenses

GPL version 3.0. See [LICENSE](https://github.com/toku-sa-n/blob-tile-generator/LICENSE)
