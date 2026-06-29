# Okabe-Ito colour palette

Palette generator for the Okabe-Ito colours, for use with ggplot2
scales. Vendored from the see package.

## Usage

``` r
palette_okabeito(palette = "full", reverse = FALSE, order = 1:9)
```

## Arguments

- palette:

  One of `"full"`, `"black_first"`, `"full_original"` or
  `"black_original"`.

- reverse:

  Reverse the colour order.

- order:

  Integer vector (within `1:9`) selecting and reordering colours.

## Value

A function that returns `n` hex colours.
