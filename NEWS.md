# anivis (development version)

## Added

* Every exported function now has a runnable example (#23).

## Fixed

* The confidence plot puts the finest-grained identity on its axis, whatever that column is called (#21). It previously looked for a column named `keypoint` and, failing that, fell back to the *coarsest* varying grouping column — the opposite of what the code's own comment described. On a frame declaring identity as, say, `animal` and `bodypart`, the axis and the facets came out swapped, rendering without error but transposed. Frames using `keypoint` are unaffected.

  The identity columns are read from the `variables_what` declaration that anicheck carries through to the check object. Check objects from an earlier anicheck carry no declaration; for those, `keypoint` remains the best guess available.

# anivis 0.2.0 (2026-06-29)

First substantial release of the plotting layer, built on aniframe (>= 0.6.0).

## Added

* `plot_trajectory()` draws x/y paths with an adaptive colour scheme — hue per `what`, shade per `when` — a time legend, gap bridging, and start and end markers.
* `plot_timeseries()` plots any per-frame numeric variable against time, inline or faceted.
* `plot_events()` and `plot.anievent()`, with `geom_event_state()` and `geom_event_point()`, draw state and point events — ethograms and spike rasters — on an hms time axis.
* Presentation methods for the `check_*()` objects produced by anicheck, dispatched via the `anivis_check_*` classes and staged by a shared `as_plot_data()` generic: `plot.anivis_check_na_timing()` for the distribution of missing values over time, `plot.anivis_check_na_gapsize()` for gap-size occurrence and totals, and `plot.anivis_check_confidence()` for per-keypoint tracking confidence as clipped horizontal violins.
* `theme_animovement()`, in light and dark, and `theme_imputets()` for the check plots.
* The Okabe-Ito and Material colour palettes with their `scale_*()` functions, and `plots()`, a patchwork wrapper.

# anivis 0.1.0

Package skeleton. No user-facing functions yet — the plotting layer arrives in 0.2.0.
