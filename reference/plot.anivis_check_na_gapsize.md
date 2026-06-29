# Plot the Occurrence of Missing-Value Gap Sizes

Renders a `check_na_gapsize()` result (from the anicheck package) as a
horizontal bar chart of how often each gap size occurs — an adaptation
of `ggplot_na_gapsize()` from the imputeTS package (Moritz & Gatscha,
GPL-3). For every gap length it draws the number of such gaps
(indianred) and, optionally, the total missing frames they account for
(steelblue, the gap size times its count). Bars are ranked so the most
common (or most costly) gaps sit on top; only the top `limit` are shown.
With more than one group, each gets its own panel.

## Usage

``` r
# S3 method for class 'anivis_check_na_gapsize'
plot(
  x,
  ...,
  ranked_by = c("occurrence", "total"),
  limit = 10,
  include_total = TRUE,
  mode = c("light", "dark")
)
```

## Arguments

- x:

  A `check_na_gapsize` object (from the anicheck package).

- ...:

  Additional arguments (currently unused).

- ranked_by:

  Order bars by `"occurrence"` (default, gap frequency) or `"total"`
  (resulting missing frames).

- limit:

  Maximum number of gap sizes (bars) to show per group, keeping the
  top-ranked. Default `10`.

- include_total:

  Whether to add the steelblue “resulting NAs” bar alongside the
  occurrence bar. Default `TRUE`.

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](http://animovement.dev/anivis/reference/theme_animovement.md).

## Value

A ggplot object.

## Details

The plot is built from an intermediate frame of class
`anivis_check_na_gapsize_data` produced by
[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
— the staging step that mirrors `data_plot()` in see.

## See also

[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
