# Plot Movement Trajectory

Creates a ggplot of the x-y trajectory from an anipoint. One path is
drawn per trajectory group, where a group is the combination of the
frame's grouping columns
([`anicore::get_keys()`](https://animovement.dev/anicore/reference/get_keys.html)):
its identity columns and its temporal context, such as session or trial.

## Usage

``` r
plot_trajectory(data, ...)

# Default S3 method
plot_trajectory(data, ..., mode = c("light", "dark"), palette = "Dark 3")
```

## Arguments

- data:

  An anipoint object.

- ...:

  Additional arguments (currently unused).

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md).

- palette:

  Name of a qualitative palette accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html);
  controls the hue family across grouping levels.

## Value

A ggplot object.

## Details

The positions are read from the columns the frame declares for its `x`
and `y` axes
([`anicore::get_axes()`](https://animovement.dev/anicore/reference/get_axes.html)),
and time from its index column
([`anicore::get_index()`](https://animovement.dev/anicore/reference/get_index.html)),
so neither has to be named `x`, `y` or `time`. The axes are labelled by
role. A three-dimensional frame is drawn in its x-y plane. A frame
without `x` and `y` axes, such as a polar one, is an error.

Colours adapt to the dataset shape:

- **single** trajectory (no grouping): the line is coloured continuously
  by time using the Material gradient scale
  ([`scale_colour_material_c()`](https://animovement.dev/anivis/reference/scale_material.md)),
  shown as a `time` colour bar.

- **what-only** or **when-only** grouping (one varying axis): each line
  gets its own hue from a qualitative palette, with `time` mapped to
  alpha so the line fades in from start to end — shown in a `time`
  legend.

- **matrix** grouping (both axes vary): each line is solid, coloured by
  the hue × shade matrix from
  [`palette_animovement()`](https://animovement.dev/anivis/reference/palette_animovement.md)
  — hue per `what`, shade per `when`; time reads from the start/end
  markers.

Every trajectory is annotated with a filled circle at its first point
and a filled triangle at its last point, identified in a start/end
legend. Gaps from missing data are bridged with a dashed line so the
path stays traceable.

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 1)
plot_trajectory(af)

```
