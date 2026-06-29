# Changelog

## anivis 0.2.0

First substantial release of the plotting layer, built on aniframe (\>=
0.6.0).

### Core plots

- [`plot_trajectory()`](http://animovement.dev/anivis/reference/plot_trajectory.md)
  — x/y paths with an adaptive colour scheme (hue per `what`, shade per
  `when`), a time legend, gap bridging, and start/end markers.
- [`plot_timeseries()`](http://animovement.dev/anivis/reference/plot_timeseries.md)
  — any per-frame numeric variable against time, inline or faceted.
- [`plot_events()`](http://animovement.dev/anivis/reference/plot_events.md)
  /
  [`plot.anievent()`](http://animovement.dev/anivis/reference/plot.anievent.md)
  with
  [`geom_event_state()`](http://animovement.dev/anivis/reference/geom_event_state.md)
  /
  [`geom_event_point()`](http://animovement.dev/anivis/reference/geom_event_point.md)
  — state and point events (ethograms / spike rasters) on an hms time
  axis.

### Diagnostic check plots (easystats-style)

Presentation methods for the `check_*()` objects produced by the
companion **anicheck** package, dispatched via the `anivis_check_*`
class and staged by a shared
[`as_plot_data()`](http://animovement.dev/anivis/reference/as_plot_data.md)
generic.

- [`plot.anivis_check_na_timing()`](http://animovement.dev/anivis/reference/plot.anivis_check_na_timing.md)
  — missing-value distribution over time.
- [`plot.anivis_check_na_gapsize()`](http://animovement.dev/anivis/reference/plot.anivis_check_na_gapsize.md)
  — gap-size occurrence / total bars.
- [`plot.anivis_check_confidence()`](http://animovement.dev/anivis/reference/plot.anivis_check_confidence.md)
  — per-keypoint tracking confidence as clipped horizontal violins.

### Foundation

- [`theme_animovement()`](http://animovement.dev/anivis/reference/theme_animovement.md)
  (light/dark) and
  [`theme_imputets()`](http://animovement.dev/anivis/reference/theme_imputets.md)
  for the check plots.
- Vendored Okabe-Ito and Material colour palettes with their `scale_*`
  functions, and a
  [`plots()`](http://animovement.dev/anivis/reference/plots.md)
  patchwork wrapper.
