# Draw state events as horizontal bars

A ggplot2 layer that renders state (durative) events as rectangles
spanning each event's `start` -\> `stop` on the x axis, with one row per
`label` on the y axis. When the input has a `type` column (e.g. an
[`aniframe::anievent()`](http://animovement.dev/aniframe/reference/anievent.md)),
rows with `type != "state"` are dropped.

## Usage

``` r
geom_event_state(
  mapping = NULL,
  data = NULL,
  ...,
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
  parent `ggplot()` call. Either way, rows with `type != "state"` are
  dropped from this layer.

- height:

  Vertical extent of each bout bar, in y-axis units (1 = the spacing
  between adjacent label rows). Default 0.7.

- na.rm, show.legend, inherit.aes, ...:

  Passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

## Value

A ggplot2 layer.

## Details

The default mapping is only `aes(xmin = start, xmax = stop)` — every
other aesthetic is up to the caller. Common patterns:

- `aes(fill = label)` — colour each bout's interior by behaviour.

- `aes(colour = label)` — outline each bout (keeps sub-pixel-wide bouts
  visible at any zoom).

- `aes(y = label)` (or `y = channel`, `y = subject`, ...) — spread bouts
  across rows. With no `y` mapping all bouts collapse onto a single
  line.

- `+ ggplot2::facet_wrap(~ channel, scales = "free_y", ncol = 1)` —
  split channels into their own panels.

Opinionated wrappers (themes, colour scales, sensible defaults) are
intended to live one layer up; this geom is deliberately bare.

This is a thin wrapper around
[ggplot2::GeomRect](https://ggplot2.tidyverse.org/reference/Geom.html):
a subclass adds a `setup_data` step that derives `ymin` / `ymax` from
`y` + `height`, so a discrete y scale keeps the label rows ranked
correctly (including under per-panel `scales = "free_y"`).
