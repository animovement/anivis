# Package index

## Plotting

- [`plot(`*`<aniframe>`*`)`](https://animovement.dev/anivis/reference/plot.aniframe.md)
  : Plot an aniframe Object
- [`plot(`*`<anievent>`*`)`](https://animovement.dev/anivis/reference/plot.anievent.md)
  : Plot an anievent Object
- [`plot_trajectory()`](https://animovement.dev/anivis/reference/plot_trajectory.md)
  : Plot Movement Trajectory
- [`plot_timeseries()`](https://animovement.dev/anivis/reference/plot_timeseries.md)
  : Plot a Variable as a Time Series
- [`plot_events()`](https://animovement.dev/anivis/reference/plot_events.md)
  : Plot State and Point Events
- [`plots()`](https://animovement.dev/anivis/reference/plots.md) :
  Arrange multiple plots side by side

## Diagnostic check plots

- [`plot(`*`<anivis_check_na_timing>`*`)`](https://animovement.dev/anivis/reference/plot.anivis_check_na_timing.md)
  : Plot the Distribution of Missing Values Over Time
- [`plot(`*`<anivis_check_na_gapsize>`*`)`](https://animovement.dev/anivis/reference/plot.anivis_check_na_gapsize.md)
  : Plot the Occurrence of Missing-Value Gap Sizes
- [`plot(`*`<anivis_check_confidence>`*`)`](https://animovement.dev/anivis/reference/plot.anivis_check_confidence.md)
  : Plot the Distribution of Tracking Confidence
- [`as_plot_data()`](https://animovement.dev/anivis/reference/as_plot_data.md)
  : Prepare a Check Result for Plotting

## Geoms

- [`geom_event_state()`](https://animovement.dev/anivis/reference/geom_event_state.md)
  : Draw state events as horizontal bars
- [`geom_event_point()`](https://animovement.dev/anivis/reference/geom_event_point.md)
  : Draw point events as dots or raster ticks

## Scales

- [`scale_colour_okabeito()`](https://animovement.dev/anivis/reference/scale_okabeito.md)
  [`scale_color_okabeito()`](https://animovement.dev/anivis/reference/scale_okabeito.md)
  [`scale_fill_okabeito()`](https://animovement.dev/anivis/reference/scale_okabeito.md)
  [`scale_colour_oi()`](https://animovement.dev/anivis/reference/scale_okabeito.md)
  [`scale_color_oi()`](https://animovement.dev/anivis/reference/scale_okabeito.md)
  [`scale_fill_oi()`](https://animovement.dev/anivis/reference/scale_okabeito.md)
  : Okabe-Ito colour scales
- [`scale_colour_material()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_color_material()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_fill_material()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_colour_material_d()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_color_material_d()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_colour_material_c()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_color_material_c()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_fill_material_d()`](https://animovement.dev/anivis/reference/scale_material.md)
  [`scale_fill_material_c()`](https://animovement.dev/anivis/reference/scale_material.md)
  : Material design colour scales

## Colour palettes

- [`okabeito_colors()`](https://animovement.dev/anivis/reference/okabeito_colors.md)
  [`oi_colors()`](https://animovement.dev/anivis/reference/okabeito_colors.md)
  : Okabe-Ito colours
- [`palette_okabeito()`](https://animovement.dev/anivis/reference/palette_okabeito.md)
  : Okabe-Ito colour palette
- [`material_colors()`](https://animovement.dev/anivis/reference/material_colors.md)
  : Material design colours
- [`palette_material()`](https://animovement.dev/anivis/reference/palette_material.md)
  : Material design colour palette
- [`palette_animovement()`](https://animovement.dev/anivis/reference/palette_animovement.md)
  : Per-group colour palette for an aniframe

## Themes

- [`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md)
  [`theme_animovement_light()`](https://animovement.dev/anivis/reference/theme_animovement.md)
  [`theme_animovement_dark()`](https://animovement.dev/anivis/reference/theme_animovement.md)
  : Animovement ggplot2 Theme
- [`theme_imputets()`](https://animovement.dev/anivis/reference/theme_imputets.md)
  : imputeTS-style Theme for Check Plots
