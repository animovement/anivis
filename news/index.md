# Changelog

## anivis 0.2.1 (2026-08-28)

### Changed

- The minimum `anicore` is 0.8.0, which is the first version published
  under that name. The constraint read `>= 0.6.0` — a version of
  `anicore` that never existed, carried over unchanged from `aniframe`
  when the dependency was renamed.

- The core data structures come from `anicore`, which is what the
  `aniframe` package was renamed to in its 0.8.0
  (animovement/anicore#84). The `aniframe` class keeps its name; only
  the package providing it changed, so `anicore` replaces `aniframe` in
  `Imports` and in every `aniframe::` call.

### Added

- Every exported function now has a runnable example
  ([\#23](https://github.com/animovement/anivis/issues/23)).

### Fixed

- `AGENTS.md` is kept out of the built package, which `R CMD check`
  reported as a non-standard top-level file.

- The confidence plot puts the finest-grained identity on its axis,
  whatever that column is called
  ([\#21](https://github.com/animovement/anivis/issues/21)). It
  previously looked for a column named `keypoint` and, failing that,
  fell back to the *coarsest* varying grouping column — the opposite of
  what the code’s own comment described. On a frame declaring identity
  as, say, `animal` and `bodypart`, the axis and the facets came out
  swapped, rendering without error but transposed. Frames using
  `keypoint` are unaffected.

  The identity columns are read from the `variables_what` declaration
  that anicheck carries through to the check object. Check objects from
  an earlier anicheck carry no declaration; for those, `keypoint`
  remains the best guess available.

## anivis 0.2.0 (2026-06-29)

First substantial release of the plotting layer, built on aniframe (\>=
0.6.0).

### Added

- [`plot_trajectory()`](https://animovement.dev/anivis/reference/plot_trajectory.md)
  draws x/y paths with an adaptive colour scheme — hue per `what`, shade
  per `when` — a time legend, gap bridging, and start and end markers.
- [`plot_timeseries()`](https://animovement.dev/anivis/reference/plot_timeseries.md)
  plots any per-frame numeric variable against time, inline or faceted.
- [`plot_events()`](https://animovement.dev/anivis/reference/plot_events.md)
  and
  [`plot.anievent()`](https://animovement.dev/anivis/reference/plot.anievent.md),
  with
  [`geom_event_state()`](https://animovement.dev/anivis/reference/geom_event_state.md)
  and
  [`geom_event_point()`](https://animovement.dev/anivis/reference/geom_event_point.md),
  draw state and point events — ethograms and spike rasters — on an hms
  time axis.
- Presentation methods for the `check_*()` objects produced by anicheck,
  dispatched via the `anivis_check_*` classes and staged by a shared
  [`as_plot_data()`](https://animovement.dev/anivis/reference/as_plot_data.md)
  generic:
  [`plot.anivis_check_na_timing()`](https://animovement.dev/anivis/reference/plot.anivis_check_na_timing.md)
  for the distribution of missing values over time,
  [`plot.anivis_check_na_gapsize()`](https://animovement.dev/anivis/reference/plot.anivis_check_na_gapsize.md)
  for gap-size occurrence and totals, and
  [`plot.anivis_check_confidence()`](https://animovement.dev/anivis/reference/plot.anivis_check_confidence.md)
  for per-keypoint tracking confidence as clipped horizontal violins.
- [`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md),
  in light and dark, and
  [`theme_imputets()`](https://animovement.dev/anivis/reference/theme_imputets.md)
  for the check plots.
- The Okabe-Ito and Material colour palettes with their `scale_*()`
  functions, and
  [`plots()`](https://animovement.dev/anivis/reference/plots.md), a
  patchwork wrapper.

## anivis 0.1.0

Package skeleton. No user-facing functions yet — the plotting layer
arrives in 0.2.0.
