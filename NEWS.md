# anivis 0.2.0

First substantial release of the plotting layer, built on aniframe (>= 0.6.0).

## Core plots

* `plot_trajectory()` — x/y paths with an adaptive colour scheme (hue per
  `what`, shade per `when`), a time legend, gap bridging, and start/end markers.
* `plot_timeseries()` — any per-frame numeric variable against time, inline or
  faceted.
* `plot_events()` / `plot.anievent()` with `geom_event_state()` /
  `geom_event_point()` — state and point events (ethograms / spike rasters) on
  an hms time axis.

## Diagnostic check plots (easystats-style)

Presentation methods for the `check_*()` objects produced by the companion
**anicheck** package, dispatched via the `anivis_check_*` class and staged by a
shared `as_plot_data()` generic.

* `plot.anivis_check_na_timing()` — missing-value distribution over time.
* `plot.anivis_check_na_gapsize()` — gap-size occurrence / total bars.
* `plot.anivis_check_confidence()` — per-keypoint tracking confidence as clipped
  horizontal violins.

## Foundation

* `theme_animovement()` (light/dark) and `theme_imputets()` for the check plots.
* Vendored Okabe-Ito and Material colour palettes with their `scale_*`
  functions, and a `plots()` patchwork wrapper.
