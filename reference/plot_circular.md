# Plot the Distribution of an Angle

**\[experimental\]**

Draws an angular column of an aniframe as a circular histogram, or rose
diagram: one wedge per bin of directions, its size the share of the
group's angles that fall in the bin. Use it for any angle the frame
stores in its angular unit, such as `course` from
[`animetric::calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.html)
or a 2D orientation (`heading`) declared with
[`animetric::add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.html).

The plot takes its conventions from the frame rather than assuming them:

- **Unit:** the column is read in the frame's `unit_angle`
  ([`anicore::get_metadata()`](https://animovement.dev/anicore/reference/get_metadata.html))
  and binned in radians
  ([`anicore::angle_to_rad()`](https://animovement.dev/anicore/reference/angle_to_rad.html)).
  The angular axis is labelled in degrees or radians to match, and
  `binwidth` is given in the same unit. A frame declaring no angular
  unit is read as radians.

- **Zero and sense:** `0` points along `+x`, drawn to the right, and
  angles count from `x` toward `y`. Whether that is counter-clockwise or
  clockwise on the page comes from
  [`anicore::get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.html):
  a frame with `y` pointing up is drawn counter-clockwise, and one with
  `y` pointing down, such as image coordinates, clockwise. A frame whose
  axis directions are unknown is drawn counter-clockwise, the sense of
  [`atan2()`](https://rdrr.io/r/base/Trig.html).

- **Range:** angles are wrapped onto the circle before binning, so
  signed angles in `(-pi, pi]`, angles in `[0, 2*pi)` and unwrapped
  angles all give the same plot, and a distribution straddling `pi` is
  not split. The axis is labelled in the range the column uses: signed
  when it holds negative angles, otherwise from `0` to a full turn.

The bins are centred on multiples of the bin width, starting at `0`, so
with the default 36 bins of 10 degrees the four directions along the
axes each fall in the middle of a bin.

Wedges show the share of each group's angles, or with `weight`, the
share of its total weight. They start from an empty inner circle,
`inner_radius` of the way out, which keeps the narrow tips of the wedges
from crowding the centre; set it to `0` for a classic rose. By default a
wedge's area, rather than its length, is proportional to its share, as
in `circular::rose.diag()` and `tectonicr::rose()`: a wedge of share `n`
reaches from the inner radius `r0` out to `sqrt(r0^2 + k * n)`, with `k`
set so that the largest reaches the edge. With `equal_area = FALSE` its
length is proportional to its share instead, which makes peaks stand out
more but exaggerates them. The radial axis is labelled in shares either
way. Missing angles and weights are left out.

Weighting is useful for directions of travel, such as `course`: when an
animal is nearly still, its direction of travel is set by tracking
jitter, and those frames can swamp the ones where it is going somewhere.
Weighting by `speed` counts each frame by how far it moved.

With `show_mean = TRUE`, a thin line marks each group's circular mean
direction
([`anicore::circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.html),
weighted when `weight` is given), running from the inner circle to the
edge. It shows the direction only, not how concentrated the angles are
around it. When the angles are close to uniform, the mean resultant
length is small and the mean direction is poorly defined, so read the
line together with the wedges.

Groups are the frame's trajectory groups
([`anicore::get_keys()`](https://animovement.dev/anicore/reference/get_keys.html)),
coloured with
[`palette_animovement()`](https://animovement.dev/anivis/reference/palette_animovement.md)
as in
[`plot_timeseries()`](https://animovement.dev/anivis/reference/plot_timeseries.md).
The `layout` argument chooses how they are arranged:

- `"facet"` (default): each group gets its own panel, filled in its
  colour –
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  when both an identity and a temporal context (such as trial) vary,
  otherwise
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- `"inline"`: all groups share one panel, each drawn as an outline in
  its colour, with a legend when there is more than one group.

## Usage

``` r
plot_circular(data, ...)

# Default S3 method
plot_circular(
  data,
  variable = NULL,
  ...,
  bins = NULL,
  binwidth = NULL,
  weight = NULL,
  equal_area = TRUE,
  inner_radius = 0.25,
  show_mean = TRUE,
  layout = c("facet", "inline"),
  mode = c("light", "dark"),
  palette = "Dark 3"
)
```

## Arguments

- data:

  An aniframe object.

- ...:

  Additional arguments (currently unused).

- variable:

  Name of the angular column to plot, a single string. Required (no
  auto-detection).

- bins:

  Number of bins round the full circle, a single whole number. Defaults
  to `36`, bins of 10 degrees, unless `binwidth` is given.

- binwidth:

  Width of each bin, in the frame's angular unit, as an alternative to
  `bins`. It must divide a full turn into a whole number of bins, for
  example `15` in a frame in degrees or `pi / 6` in one in radians.

- weight:

  Name of a numeric column to weight each angle by, such as `"speed"`,
  or `NULL` (default) to count each angle once. Weights must not be
  negative.

- equal_area:

  Whether a wedge's area (`TRUE`, default) or its length (`FALSE`) is
  proportional to its share.

- inner_radius:

  Radius of the empty circle in the middle, as a fraction of the plot's
  radius, from `0` (a classic rose) up to but not including `1`. Default
  `0.25`.

- show_mean:

  Whether to draw a line at each group's circular mean direction.
  Default `TRUE`.

- layout:

  Either `"facet"` (default, one panel per group) or `"inline"` (all
  groups in one panel, as outlines).

- mode:

  Either `"light"` (default) or `"dark"`; passed to
  [`theme_animovement()`](https://animovement.dev/anivis/reference/theme_animovement.md).

- palette:

  Name of a qualitative palette accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html);
  controls the hue family across groups.

## Value

A ggplot object.

## See also

[`plot_timeseries()`](https://animovement.dev/anivis/reference/plot_timeseries.md)
for the same column against time.

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 200, n_individuals = 2, n_keypoints = 1)
af$course <- atan2(c(0, diff(af$y)), c(0, diff(af$x)))
plot_circular(af, variable = "course")


# Fewer, wider bins, with the groups overlaid
plot_circular(af, variable = "course", binwidth = pi / 6, layout = "inline")


# Course from animetric, and a heading declared from two keypoints, in a
# frame storing angles in degrees
kin <- anicore::example_anipoint(n_obs = 200, n_individuals = 1) |>
  anicore::convert_unit_angle("deg") |>
  animetric::add_orientation(
    from = "shoulder_right",
    to = "head",
    level = "keypoint"
  ) |>
  animetric::calculate_kinematics()

# Each frame counted by how far it moved
plot_circular(kin, variable = "course", weight = "speed")

plot_circular(kin, variable = "heading", show_mean = FALSE)
```
