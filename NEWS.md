# anivis (development version)

## Fixed

* `plot()` works on an aniframe with nothing to draw (#32) — one with no rows, or one whose positions are all `NA`. Both failed with `arguments imply differing number of rows: 0, 2`, preceded by a warning about the `each` argument, neither of which pointed at the frame. The second case is the likelier one: a keypoint the tracker never found has no start or end to mark.

  `trajectory_endpoints()` returned `NULL` when no group had a valid point — `do.call(rbind, list())` — and `nrow(NULL)` then reached `rep(each = )`, which used a `NULL` length and produced two rows against a column of none.

# anivis 0.2.1 (2026-08-28)

## Changed

* The minimum `anicore` is 0.8.0, which is the first version published under that name. The constraint read `>= 0.6.0` — a version of `anicore` that never existed, carried over unchanged from `aniframe` when the dependency was renamed.

* The core data structures come from `anicore`, which is what the `aniframe` package was renamed to in its 0.8.0 (animovement/anicore#84). The `aniframe` class keeps its name; only the package providing it changed, so `anicore` replaces `aniframe` in `Imports` and in every `aniframe::` call.

## Added

* Every exported function now has a runnable example (#23).

## Fixed

* `AGENTS.md` is kept out of the built package, which `R CMD check` reported as a non-standard top-level file.

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
