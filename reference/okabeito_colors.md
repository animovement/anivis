# Okabe-Ito colours

Extract one or more colours from the colour-blind-safe Okabe-Ito
palette. `oi_colors()` is an alias. Vendored from the see package.

## Usage

``` r
okabeito_colors(..., original_names = FALSE, black_first = FALSE, amber = TRUE)

oi_colors(..., original_names = FALSE, black_first = FALSE, amber = TRUE)
```

## Arguments

- ...:

  Character names of colours to extract (e.g. `"orange"`). If none are
  given, the full ordered palette is returned.

- original_names:

  Use the original Okabe & Ito colour names (e.g. `"sky blue"`,
  `"vermillion"`) rather than the plain-language ones.

- black_first:

  Place black first in the returned palette.

- amber:

  Use the slightly darkened "amber" yellow (`#F5C710`) instead of the
  original bright yellow (`#F0E442`), for better legibility.

## Value

A named character vector of hex colours.
