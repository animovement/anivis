# Per-group colour palette for an aniframe

Builds a named character vector mapping trajectory group keys to
colours. The colour scheme adapts to how many `what` and (non-time)
`when` levels the aniframe has:

## Usage

``` r
palette_animovement(data, palette = "Dark 3", single_hue = "#3A6FB0")
```

## Arguments

- data:

  An aniframe.

- palette:

  Name of a qualitative palette accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html).
  Default `"Dark 3"`.

- single_hue:

  Colour used when neither `what` nor `when` has more than one level.

## Value

A named character vector of hex colours.

## Details

- **single** (one of each, or none): a single hue.

- **what** (multiple `what`, one `when`): one qualitative hue per `what`
  level.

- **when** (one `what`, multiple `when`): one qualitative hue per `when`
  level.

- **matrix** (multiple of both): one hue per `what` level, then a
  lighter-to-darker shade of that hue per `when` level.

Names of the returned vector match the group keys returned by the
internal grouping logic — the redundant axis is dropped when only one
side varies.
