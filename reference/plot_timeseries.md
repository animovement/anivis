# Plot a Variable as a Time Series

Plots one or more numeric variables from an aniframe against `time`,
with one line per trajectory group (every `variables_what` column and
every non-time `variables_when` column in the metadata). Use it for any
per-frame measure — `speed`, `acceleration`, a confidence score, a
temperature channel, or any other derived column.

## Usage

``` r
plot_timeseries(data, ...)

# Default S3 method
plot_timeseries(
  data,
  variable = NULL,
  ...,
  layout = c("inline", "facet"),
  mode = c("light", "dark"),
  palette = "Dark 3"
)
```

## Arguments

- data:

  An aniframe object.

- ...:

  Additional arguments (currently unused).

- variable:

  Name(s) of the column(s) to plot on the y axis — a character vector.
  Required (no auto-detection). Several names draw a stacked panel per
  variable.

- layout:

  Either `"inline"` (default, all groups in one panel) or `"facet"` (one
  panel per group).

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](http://animovement.dev/anivis/reference/theme_animovement.md).

- palette:

  Name of a qualitative palette accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html);
  controls the hue family across groups.

## Value

A ggplot object for a single variable, or a stacked patchwork for
several.

## Details

Pass several names to `variable` to draw a panel per variable,
**vertically stacked** with a shared x axis and a single shared legend
(via [`plots()`](http://animovement.dev/anivis/reference/plots.md));
stacking makes it easy to compare their shape over time. A single
variable returns a plain ggplot.

The `layout` argument chooses how groups are arranged, mirroring
[`plot_events()`](http://animovement.dev/anivis/reference/plot_events.md):

- `"inline"` (default): all groups share one panel as coloured lines,
  with a legend when there is more than one group.

- `"facet"`: each group gets its own panel — `facet_grid()` when both an
  identity and a (non-time) condition vary, otherwise `facet_wrap()`
  with the panels stacked in rows so the shared x axis lines up.

Colours come from
[`palette_animovement()`](http://animovement.dev/anivis/reference/palette_animovement.md),
matching
[`plot_trajectory()`](http://animovement.dev/anivis/reference/plot_trajectory.md).
The x axis uses the same time-unit handling as
[`plot_events()`](http://animovement.dev/anivis/reference/plot_events.md)
— `HH:MM:SS` for true time units, raw frames otherwise.
