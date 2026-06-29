# Plot an aniframe Object

Creates a visualization of movement data stored in an aniframe. Returns
a patchwork object that can be combined with additional plots.

## Usage

``` r
# S3 method for class 'aniframe'
plot(x, ..., mode = c("light", "dark"))
```

## Arguments

- x:

  An aniframe object.

- ...:

  Additional arguments passed to underlying plot functions.

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`plot_trajectory()`](http://animovement.dev/anivis/reference/plot_trajectory.md).

## Value

A patchwork object.
