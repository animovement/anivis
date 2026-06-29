# Draw point events as dots or raster ticks

A ggplot2 layer that renders instantaneous events at each event's
`start` time, with one row per `label` (or `channel`, ...) on the y
axis. Two styles are available via `style`:

## Usage

``` r
geom_event_point(
  mapping = NULL,
  data = NULL,
  ...,
  style = c("point", "raster"),
  size = 1.8,
  height = 0.7,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Aesthetic mapping. Merged on top of the defaults, so you only need to
  supply the aesthetics you want to override.

- data:

  A data frame (typically an `anievent`). If `NULL`, inherits from the
  parent `ggplot()` call. Either way, rows with `type != "point"` are
  dropped from this layer.

- style:

  Either `"point"` (default, a dot per event) or `"raster"` (a vertical
  tick per event).

- size:

  Dot size, for `style = "point"`. Default 1.8.

- height:

  Tick height in y-axis units (1 = the spacing between adjacent rows),
  for `style = "raster"`. Default 0.7.

- na.rm, show.legend, inherit.aes, ...:

  Passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

## Value

A ggplot2 layer.

## Details

- `"point"` (default) — a dot at each event time.

- `"raster"` — a short vertical tick at each event time, the classic
  spike-raster look (more standard for e.g. neuron spike trains).

When the input has a `type` column (e.g. an
[`aniframe::anievent()`](http://animovement.dev/aniframe/reference/anievent.md)),
rows with `type != "point"` are dropped — the point half of the
state/point pair complementing
[`geom_event_state()`](http://animovement.dev/anivis/reference/geom_event_state.md).

The default mapping is only `aes(x = start)` — every other aesthetic is
up to the caller. Common patterns:

- `aes(colour = label)` — colour each mark by behaviour.

- `aes(y = label)` (or `y = channel`, `y = subject`, ...) — spread marks
  across rows. With no `y` mapping all marks collapse onto a single
  line.

Opinionated wrappers (themes, colour scales, sensible defaults) are
intended to live one layer up; this geom is deliberately bare.

This is a thin wrapper around
[ggplot2::GeomPoint](https://ggplot2.tidyverse.org/reference/Geom.html)
(`"point"`) or
[ggplot2::GeomSegment](https://ggplot2.tidyverse.org/reference/Geom.html)
(`"raster"`), with a `setup_data` step that supplies a default `y` when
none is mapped.
