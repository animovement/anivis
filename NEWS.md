# anivis (development version)

## Added

* `plot_circular()` draws an angular column, such as `course` from animetric or a declared `heading`, as a circular histogram (rose diagram) per trajectory group (#38). It takes its conventions from the frame: angles are read in the frame's `unit_angle` and the axis labelled in degrees or radians to match, 0 points along +x and angles run counter-clockwise or clockwise as `anicore::get_angle_direction()` says, and angles in either (-pi, pi] or [0, 2*pi) are wrapped so a distribution straddling pi is not split. Wedges start from an empty inner circle and their area is proportional to their share by default (`inner_radius`, `equal_area`). The bins can be set as a count (`bins`, 36 by default) or a width in the frame's unit (`binwidth`), angles can be weighted by another column (`weight`, e.g. `"speed"`, so that frames where the animal is nearly still do not dominate), and a thin line from the inner circle to the edge marks each group's circular mean direction. It is experimental.

* `plot()` draws a `check_segment_length()` result from anicheck (animovement/anicheck#39): a horizontal violin per segment of its length relative to its reference, with the median and inter-quartile range, a line at the reference and dashed lines at the check's tolerance either side, and a panel per individual. Unstable segments are the wide violins, or those reaching past the dashed lines. `as_plot_data()` gives the data it is drawn from. It is experimental, like the check.

## Changed

* The minimum `anicore` is 0.8.0.9004, the first with `angle_to_rad()`, and the minimum `ggplot2` is 3.5.0, the first with `coord_radial()`. Both are for `plot_circular()`. `animetric` is suggested, for its examples and tests.

* Works with anicore's `anipoint` class and rebuilt accessor API (animovement/anicore#154). `plot()` dispatches on `anipoint` rather than `aniframe`, and `plot_trajectory()` requires an `anipoint`.

* Functions now carry a lifecycle stage (animovement/.github#46), and unlabelled means stable. `plot()` on an anipoint (`plot.anipoint()`) is labelled experimental: it draws the trajectory alone today, and what it shows may change without a deprecation cycle, for example to add speed and course traces beneath the path (#9). Call `plot_trajectory()` directly when you need exactly that plot. Every other function is stable, and changes only through a deprecation cycle.

## Fixed

* `plot_trajectory()`, `plot_timeseries()` and `plot()` read the positions and time from the columns the frame declares for them (#37), so the axes and the index no longer have to be called `x`, `y` and `time`. A frame with renamed axes failed with ``Column `x` not found``, and one with a renamed index with `argument 2 is not a vector`. The axes are still labelled by role, as `x (mm)`, and frames using the standard names plot exactly as before.

  The x and y axes come from `anicore::get_axes()`, so a three-dimensional frame is drawn in its x-y plane whatever its columns are called. A frame without x and y axes, such as a polar one, now fails saying so, instead of reporting a missing column. Time comes from `anicore::get_index()`. Trajectory groups, and the palette that colours them, come from `anicore::get_keys()`, the columns the frame is grouped by.

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
