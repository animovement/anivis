# Okabe-Ito colour scales

Colour-blind-safe qualitative colour and fill scales using the Okabe-Ito
palette, vendored from the see package. `*_oi()` are shorter aliases.

## Usage

``` r
scale_colour_okabeito(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "colour",
  ...
)

scale_color_okabeito(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "colour",
  ...
)

scale_fill_okabeito(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "fill",
  ...
)

scale_colour_oi(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "colour",
  ...
)

scale_color_oi(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "colour",
  ...
)

scale_fill_oi(
  palette = "full",
  reverse = FALSE,
  order = 1:9,
  aesthetics = "fill",
  ...
)
```

## Arguments

- palette:

  One of `"full"`, `"black_first"`, `"full_original"` or
  `"black_original"`.

- reverse:

  Reverse the colour order.

- order:

  Integer vector (within `1:9`) selecting and reordering colours.

- aesthetics:

  Aesthetics this scale applies to.

- ...:

  Passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

## Value

A ggplot2 scale.

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  scale_color_okabeito()
```
