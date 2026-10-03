# Prepare a Check Result for Plotting

Coerces a check object — such as the result of `check_na_timing()` (from
the anicheck package) — into the plot-ready data frame that its
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method draws
from. This is the staging step that sits between the diagnostic
computation (the `check_*()` function) and the ggplot: it reshapes the
natural, analysis-friendly columns of the check into the exact aesthetic
contract a geom needs (a single grouping factor, ordered fill levels,
the right level ordering, and so on).

## Usage

``` r
as_plot_data(x, ...)

# Default S3 method
as_plot_data(x, ...)

# S3 method for class 'check_confidence'
as_plot_data(x, ..., clip = 0.02)

# S3 method for class 'check_na_gapsize'
as_plot_data(
  x,
  ...,
  ranked_by = c("occurrence", "total"),
  limit = 10,
  include_total = TRUE
)

# S3 method for class 'check_na_timing'
as_plot_data(x, ..., measure = c("percent", "count"), n_intervals = NULL)
```

## Arguments

- x:

  A check object.

- ...:

  Additional arguments passed to methods.

- clip:

  For `as_plot_data.check_confidence()`: density floor (fraction of each
  keypoint's peak) below which the violin is cut. Default `0.02`.

- ranked_by, limit, include_total:

  For `as_plot_data.check_na_gapsize()`: order bars by `"occurrence"` or
  `"total"`, keep the top `limit` per group, and whether to include the
  total-NAs series.

- measure:

  For `as_plot_data.check_na_timing()`: `"percent"` (default, share of
  each interval) or `"count"` (number of frames).

- n_intervals:

  For `as_plot_data.check_na_timing()`: number of time intervals to bin
  into (default `NULL` uses Sturges' rule on the largest group).

## Value

A data frame classed for the corresponding plot method (for example
`anivis_check_na_timing_data`).

## Details

It is the anivis analog of `data_plot()` in the see package. Most users
never call it directly —
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) calls it for
you. Reach for it when you want the precise rows a plot is built from,
to inspect them or to assemble a custom chart by hand.

Methods exist for check objects with a single canonical plot (one object
class maps to one figure). Object types that can be plotted several
different ways — an aniframe, which feeds both
[`plot_trajectory()`](https://animovement.dev/anivis/reference/plot_trajectory.md)
and
[`plot_timeseries()`](https://animovement.dev/anivis/reference/plot_timeseries.md)
— deliberately do not have a single method, since there would be nothing
for it to dispatch on to choose between those shapes.

`as_plot_data.check_confidence()` turns the per-keypoint density grid
into closed violin polygons: each keypoint sits at an integer y
position, and its density is mirrored either side along the confidence
(x) axis (width-normalised so every violin has the same maximum width).
Grid points below `clip` x the peak are dropped, so thin tails and
bimodal-bridging necks disappear and a split distribution becomes
separate polygons. `keypoint` is the x-axis category; any other identity
that varies (e.g. `individual`) collapses into a `group` factor for
faceting. The x positions, a per-keypoint median / quartile `overlay`,
and `facet` ride along as attributes. Returns a frame classed
`anivis_check_confidence_data`.

`as_plot_data.check_na_gapsize()` reshapes the gap-size table into the
long, ranked form the imputeTS-style bar chart needs: one row per
(group, gap size, series), where `series` is `occurrence` (and `total`
when `include_total`), `value` the count, and `key` a
`reorder_within`-style factor (`"<n> NA-gap___ <group>"`) ordered by
`ranked_by` so each facet sorts independently. Returns a frame classed
`anivis_check_na_gapsize_data`.

`as_plot_data.check_na_timing()` reconstructs, from the compact gap
table, the missing / present frame counts per time interval — one row
per (group, interval, status). A single interval width (in frames) is
chosen for all groups (Sturges' rule by default) so bars line up across
panels, and each gap's overlap with each interval is counted, so no
per-frame data is needed. `value` is the share (`measure = "percent"`)
or count (`"count"`), `width` the interval's span in time units, and the
frame interval size rides along as an attribute. Returns a frame classed
`anivis_check_na_timing_data`.

## See also

[`plot.anivis_check_na_timing()`](https://animovement.dev/anivis/reference/plot.anivis_check_na_timing.md)

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 50, n_individuals = 1, n_keypoints = 3)
af$x[c(5:8, 20, 31:40)] <- NA

# The data the plot is drawn from, e.g. to build a custom plot from it
as_plot_data(anicheck::check_na_timing(af))
#>             group  x width  status count size     value
#> 1            head  4     7 present     4    7 0.5714286
#> 2            head 11     7 present     6    7 0.8571429
#> 3            head 18     7 present     6    7 0.8571429
#> 4            head 25     7 present     7    7 1.0000000
#> 5            head 32     7 present     2    7 0.2857143
#> 6            head 39     7 present     2    7 0.2857143
#> 7            head 46     7 present     7    7 1.0000000
#> 8            head 50     1 present     1    1 1.0000000
#> 9            neck  4     7 present     7    7 1.0000000
#> 10           neck 11     7 present     7    7 1.0000000
#> 11           neck 18     7 present     7    7 1.0000000
#> 12           neck 25     7 present     7    7 1.0000000
#> 13           neck 32     7 present     7    7 1.0000000
#> 14           neck 39     7 present     7    7 1.0000000
#> 15           neck 46     7 present     7    7 1.0000000
#> 16           neck 50     1 present     1    1 1.0000000
#> 17 shoulder_right  4     7 present     7    7 1.0000000
#> 18 shoulder_right 11     7 present     7    7 1.0000000
#> 19 shoulder_right 18     7 present     7    7 1.0000000
#> 20 shoulder_right 25     7 present     7    7 1.0000000
#> 21 shoulder_right 32     7 present     7    7 1.0000000
#> 22 shoulder_right 39     7 present     7    7 1.0000000
#> 23 shoulder_right 46     7 present     7    7 1.0000000
#> 24 shoulder_right 50     1 present     1    1 1.0000000
#> 25           head  4     7 missing     3    7 0.4285714
#> 26           head 11     7 missing     1    7 0.1428571
#> 27           head 18     7 missing     1    7 0.1428571
#> 28           head 25     7 missing     0    7 0.0000000
#> 29           head 32     7 missing     5    7 0.7142857
#> 30           head 39     7 missing     5    7 0.7142857
#> 31           head 46     7 missing     0    7 0.0000000
#> 32           head 50     1 missing     0    1 0.0000000
#> 33           neck  4     7 missing     0    7 0.0000000
#> 34           neck 11     7 missing     0    7 0.0000000
#> 35           neck 18     7 missing     0    7 0.0000000
#> 36           neck 25     7 missing     0    7 0.0000000
#> 37           neck 32     7 missing     0    7 0.0000000
#> 38           neck 39     7 missing     0    7 0.0000000
#> 39           neck 46     7 missing     0    7 0.0000000
#> 40           neck 50     1 missing     0    1 0.0000000
#> 41 shoulder_right  4     7 missing     0    7 0.0000000
#> 42 shoulder_right 11     7 missing     0    7 0.0000000
#> 43 shoulder_right 18     7 missing     0    7 0.0000000
#> 44 shoulder_right 25     7 missing     0    7 0.0000000
#> 45 shoulder_right 32     7 missing     0    7 0.0000000
#> 46 shoulder_right 39     7 missing     0    7 0.0000000
#> 47 shoulder_right 46     7 missing     0    7 0.0000000
#> 48 shoulder_right 50     1 missing     0    1 0.0000000
```
