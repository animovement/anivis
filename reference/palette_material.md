# Material design colour palette

Palette generator for the Material design colours. Vendored from the see
package.

## Usage

``` r
palette_material(palette = "contrast", reverse = FALSE, ...)
```

## Arguments

- palette:

  One of `"full"`, `"ice"`, `"gradient"`, `"rainbow"`, `"contrast"`,
  `"light"` or `"complement"`.

- reverse:

  Reverse the colour order.

- ...:

  Additional arguments passed to the underlying palette retriever.

## Value

A function that returns `n` hex colours.
