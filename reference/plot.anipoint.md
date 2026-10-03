# Plot an anipoint Object

**\[experimental\]**

Creates a visualization of movement data stored in an
[`anicore::anipoint()`](https://animovement.dev/anicore/reference/anipoint.html).
Returns a patchwork object that can be combined with additional plots.

Today the figure is the trajectory alone, drawn by
[`plot_trajectory()`](https://animovement.dev/anivis/reference/plot_trajectory.md).
What [`plot()`](https://rdrr.io/r/graphics/plot.default.html) shows may
change without a deprecation cycle, for example to add speed and course
traces beneath the path. Call
[`plot_trajectory()`](https://animovement.dev/anivis/reference/plot_trajectory.md)
directly when you need exactly that plot.

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

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 1)
plot(af)

```
