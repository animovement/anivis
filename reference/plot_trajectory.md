# Plot Movement Trajectory

Creates a ggplot of the x-y trajectory from an aniframe. One path is
drawn per trajectory group, where a group is the combination of every
`variables_what` column and every non-time `variables_when` column in
the aniframe's metadata.

## Usage

``` r
plot_trajectory(data, ...)

# Default S3 method
plot_trajectory(data, ..., mode = c("light", "dark"), palette = "Dark 3")
```

## Arguments

- data:

  An aniframe object.

- ...:

  Additional arguments (currently unused).

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](http://animovement.dev/anivis/reference/theme_animovement.md).

- palette:

  Name of a qualitative palette accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html);
  controls the hue family across grouping levels.

## Value

A ggplot object.

## Details

Colours adapt to the dataset shape:

- **single** trajectory (no grouping): the line is coloured continuously
  by `time` using the Material gradient scale
  ([`scale_colour_material_c()`](http://animovement.dev/anivis/reference/scale_material.md)),
  shown as a `time` colour bar.

- **what-only** or **when-only** grouping (one varying axis): each line
  gets its own hue from a qualitative palette, with `time` mapped to
  alpha so the line fades in from start to end — shown in a `time`
  legend.

- **matrix** grouping (both axes vary): each line is solid, coloured by
  the hue × shade matrix from
  [`palette_animovement()`](http://animovement.dev/anivis/reference/palette_animovement.md)
  — hue per `what`, shade per `when`; time reads from the start/end
  markers.

Every trajectory is annotated with a filled circle at its first point
and a filled triangle at its last point, identified in a start/end
legend. Gaps from missing data are bridged with a dashed line so the
path stays traceable.
