# Plot the Distribution of Tracking Confidence

Renders a `check_confidence()` result (from the anicheck package) as a
horizontal violin per keypoint of its tracking confidence (keypoints on
the y axis, confidence on the x), with a point at the median and a line
spanning the inter-quartile range. A violin sitting far to the right and
tight is a reliably tracked keypoint; a left-shifted body or a long low
tail flags one the tracker was often unsure about. With several
individuals, each gets its own panel.

## Usage

``` r
# S3 method for class 'anivis_check_confidence'
plot(x, ..., clip = 0.02, mode = c("light", "dark"))
```

## Arguments

- x:

  A `check_confidence` object (from the anicheck package).

- ...:

  Additional arguments (currently unused).

- clip:

  Cut each violin where its density falls below this fraction of its
  peak, so thin tails and the neck bridging a bimodal distribution are
  removed and the violin shows only where data actually is. Default
  `0.02`; set `0` to keep the full density.

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_imputets()`](http://animovement.dev/anivis/reference/theme_imputets.md).

## Value

A ggplot object.

## Details

The violins are drawn from the kernel-density grid stored in the check
object (via `geom_polygon`), so no raw values are needed. Styling
matches the other check plots
([`theme_imputets()`](http://animovement.dev/anivis/reference/theme_imputets.md),
horizontal-only gridlines). The plot is built from an intermediate frame
of class `anivis_check_confidence_data` produced by
[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
— the staging step that mirrors `data_plot()` in see.

## See also

[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
