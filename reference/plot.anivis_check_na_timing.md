# Plot the Distribution of Missing Values Over Time

Renders a `check_na_timing()` result (from the anicheck package) as a
stacked bar chart of missing (`NA`) versus present values across
successive time intervals — an adaptation of `ggplot_na_distribution2()`
from the imputeTS package (Moritz & Gatscha, GPL-3). Each bar is one
interval of the recording; its split shows how much of that stretch was
missing, so a run of tall “NA” bars marks a blackout while an even
sprinkle marks scattered dropouts. With more than one group, each gets
its own stacked panel.

## Usage

``` r
# S3 method for class 'anivis_check_na_timing'
plot(
  x,
  ...,
  measure = c("percent", "count"),
  n_intervals = NULL,
  mode = c("light", "dark")
)
```

## Arguments

- x:

  A `check_na_timing` object (from the anicheck package).

- ...:

  Additional arguments (currently unused).

- measure:

  Either `"percent"` (default, share of each interval) or `"count"`
  (number of frames).

- n_intervals:

  Number of intervals to bin into. Default (`NULL`) uses Sturges' rule
  on the largest group's frame count.

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_imputets()`](http://animovement.dev/anivis/reference/theme_imputets.md).

## Value

A ggplot object.

## Details

The interval counts are reconstructed from the compact gap table in the
check object (no per-frame data needed). Missing uses the imputeTS
indianred, present the imputeTS steelblue, with the two named in the
coloured subtitle (so no legend is needed). Styling is
[`theme_imputets()`](http://animovement.dev/anivis/reference/theme_imputets.md).
The plot is built from an intermediate frame of class
`anivis_check_na_timing_data` produced by
[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
— the staging step that mirrors `data_plot()` in see.

## See also

[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
