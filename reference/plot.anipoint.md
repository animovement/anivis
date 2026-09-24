# Plot an anipoint Object

Creates a visualization of movement data stored in an
[`anicore::anipoint()`](https://animovement.dev/anicore/reference/anipoint.html).
Returns a patchwork object that can be combined with additional plots.

## Usage

``` r
# S3 method for class 'anipoint'
plot(x, ..., mode = c("light", "dark"))
```

## Arguments

- x:

  An anipoint object.

- ...:

  Additional arguments passed to underlying plot functions.

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`plot_trajectory()`](https://animovement.dev/anivis/reference/plot_trajectory.md).

## Value

A patchwork object.
