# Material design colour scales

Colour and fill scales using the Material design palette, vendored from
the see package. With `discrete = FALSE` the palette is interpolated
into a smooth gradient — the default `"gradient"` palette runs blue to
orange, the warm-to-cool ramp suited to continuous data.

## Usage

``` r
scale_colour_material(
  palette = NULL,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "colour",
  ...
)

scale_color_material(
  palette = NULL,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "colour",
  ...
)

scale_fill_material(
  palette = NULL,
  discrete = TRUE,
  reverse = FALSE,
  aesthetics = "fill",
  ...
)

scale_colour_material_d(palette = NULL, ...)

scale_color_material_d(palette = NULL, ...)

scale_colour_material_c(palette = NULL, ...)

scale_color_material_c(palette = NULL, ...)

scale_fill_material_d(palette = NULL, ...)

scale_fill_material_c(palette = NULL, ...)
```

## Arguments

- palette:

  Palette name (see
  [`palette_material()`](http://animovement.dev/anivis/reference/palette_material.md)).
  Defaults to `"contrast"` when discrete and `"gradient"` when
  continuous.

- discrete:

  Treat the scale as discrete (`TRUE`) or continuous (`FALSE`).

- reverse:

  Reverse the colour order.

- aesthetics:

  Aesthetics this scale applies to.

- ...:

  Passed to the underlying ggplot2 scale.

## Value

A ggplot2 scale.
