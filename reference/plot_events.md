# Plot State and Point Events

Creates a ggplot that overlays state events (durative bouts, drawn as
horizontal bars by
[`geom_event_state()`](http://animovement.dev/anivis/reference/geom_event_state.md))
and point events (instantaneous occurrences, drawn as dots by
[`geom_event_point()`](http://animovement.dev/anivis/reference/geom_event_point.md))
on a shared time axis. Each behaviour `label` becomes a row on the y
axis.

## Usage

``` r
plot_events(data, ...)

# S3 method for class 'anievent'
plot_events(
  data,
  ...,
  layout = c("facet", "inline"),
  point_style = c("point", "raster"),
  mode = c("light", "dark")
)

# Default S3 method
plot_events(
  data = NULL,
  point = NULL,
  ...,
  point_style = c("point", "raster"),
  mode = c("light", "dark")
)
```

## Arguments

- data:

  An anievent object, or a data frame of state events for the default
  method.

- ...:

  Additional arguments (currently unused).

- layout:

  For `plot_events.anievent()`: either `"facet"` (default, one y row per
  label with channels faceted) or `"inline"` (an ethogram — one y row
  per channel with labels in a colour legend).

- point_style:

  How point events are drawn: `"point"` (default, dots) or `"raster"`
  (vertical ticks, the classic spike-raster look). Passed to
  [`geom_event_point()`](http://animovement.dev/anivis/reference/geom_event_point.md).

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](http://animovement.dev/anivis/reference/theme_animovement.md).

- point:

  For the default method only: an optional data frame of point events.
  Ignored by `plot_events.anievent()`.

## Value

A ggplot object.

## Details

Two dispatches:

- **`plot_events.anievent()`** — auto-detects which rows are states and
  which are points by reading the `type` column. The `layout` argument
  chooses how channels and labels are arranged:

  - `"facet"` (default): each `label` is a y-axis row, and multiple
    `channel`s become facet rows (with free y-scales). Any
    `variables_what` column with more than one level adds a facet column
    per identity — `facet_grid()` when both vary, otherwise
    `facet_wrap()`.

  - `"inline"` (ethogram): each `channel` is a single y-axis row and
    `label` moves to a colour legend, so all channels share one panel.

- **`plot_events.default()`** — for callers without an anievent. Pass
  the state events as the first arg and the point events via `point`;
  either may be `NULL`. No automatic faceting.
