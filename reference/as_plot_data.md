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
[`plot_trajectory()`](http://animovement.dev/anivis/reference/plot_trajectory.md)
and
[`plot_timeseries()`](http://animovement.dev/anivis/reference/plot_timeseries.md)
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

[`plot.anivis_check_na_timing()`](http://animovement.dev/anivis/reference/plot.anivis_check_na_timing.md)
